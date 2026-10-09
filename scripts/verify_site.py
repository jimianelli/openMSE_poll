"""Verify local HTML links, image references, and the public release boundary."""
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urlsplit, unquote
import json
import re
import subprocess

ROOT = Path(__file__).resolve().parents[1]
DOCS = ROOT / "docs"


class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.links = []
        self.images = []
        self.ids = set()
        self.lang = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "html":
            self.lang = attrs.get("lang")
        if "id" in attrs:
            self.ids.add(attrs["id"])
        for key in ("src", "href", "poster"):
            if attrs.get(key):
                self.links.append(attrs[key])
        if tag == "img":
            self.images.append(attrs)


assert (DOCS / "index.html").is_file(), "Missing homepage"
errors = []
parsers = {}
for path in DOCS.rglob("*.html"):
    parser = Links()
    parser.feed(path.read_text())
    parsers[path] = parser
    if not parser.lang:
        errors.append(f"Missing language: {path.relative_to(ROOT)}")
    for uri in parser.links:
        link = urlsplit(uri)
        if link.scheme or link.netloc or uri.startswith(("data:", "javascript:")):
            continue
        target = (path.parent / unquote(link.path)).resolve() if link.path else path
        if not target.is_relative_to(DOCS):
            errors.append(f"Reference leaves deployment: {path.relative_to(ROOT)} -> {uri}")
        elif not target.exists():
            errors.append(f"Missing reference: {path.relative_to(ROOT)} -> {uri}")
        elif link.fragment and target.suffix == ".html":
            if target not in parsers:
                dest = Links()
                dest.feed(target.read_text())
                parsers[target] = dest
            if unquote(link.fragment) not in parsers[target].ids:
                errors.append(f"Missing fragment: {path.relative_to(ROOT)} -> {uri}")

for path in [DOCS / "reports/updated-inputs.html"]:
    for attrs in parsers[path].images:
        if not attrs.get("alt"):
            errors.append(f"Missing review chart alt text: {path.relative_to(ROOT)}")

notice = (DOCS / "reports/historical-walkthrough.html").read_text()
assert "Historical demonstration — January 2026 render" in notice
assert "Slick F/FMSY export" in notice

for path in DOCS.rglob("*.css"):
    for uri in re.findall(r"url\(['\"]?([^)'\"]+)", path.read_text()):
        if urlsplit(uri).scheme or uri.startswith(("data:", "//", "#")):
            continue
        target = (path.parent / unquote(urlsplit(uri).path)).resolve()
        if not target.is_relative_to(DOCS) or not target.exists():
            errors.append(f"Missing CSS asset: {path.relative_to(ROOT)} -> {uri}")

# Inspect actual tracked files when running in Git; otherwise inspect the site.
git = subprocess.run(["git", "ls-files", "-z"], cwd=ROOT, capture_output=True)
tracked = [ROOT / p for p in git.stdout.decode().split("\0") if p] if git.returncode == 0 else list(DOCS.rglob("*"))
for p in tracked:
    if not p.is_file():
        continue
    rel = str(p.relative_to(ROOT))
    if p.suffix.lower() in {".rds", ".xlsx", ".xls", ".rdata", ".rdb", ".rdx"} or "_cache/" in rel:
        errors.append(f"Excluded raw/cache payload tracked: {rel}")
    if p.suffix.lower() in {".r", ".qmd", ".html", ".md", ".yml", ".yaml", ".json", ".csv"}:
        txt = p.read_text()
        # This audit scans literal disclosures, not ordinary environment references.
        if re.search(r"/Users/\w|/(?:home|private/(?:tmp|var))/[A-Za-z0-9]", txt):
            errors.append(f"Machine-local path disclosed: {rel}")
        if re.search(r"(?:gh[pousr]_[A-Za-z0-9]{25,}|github_pat_[A-Za-z0-9_]{30,}|AKIA[A-Z0-9]{16}|-----BEGIN (?:RSA |OPENSSH )?PRIVATE KEY-----)", txt):
            errors.append(f"Credential pattern detected: {rel}")

result = {"html_files": len(parsers), "errors": errors,
          "review_chart_alt_text": "Pass", "screen_reader_review": "Not Tested",
          "historical_figure_accessibility": "Requires analytical revision",
          "historical_unresolved_metric_reference": "Recorded in evaluation"}
print(json.dumps(result, indent=2))
if errors:
    raise SystemExit(1)
print("SITE_VERIFICATION_PASS")

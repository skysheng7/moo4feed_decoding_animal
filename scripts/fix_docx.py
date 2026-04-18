"""Post-render fix for Quarto+flextable docx output.

Quarto wraps tables and figure captions in a way that produces two adjacent
<w:pPr> elements inside a single <w:p>, which Microsoft Word flags as
"unreadable content" (Issue: quarto-cli#7321, #10587, #12154).

This script rewrites <w:p><w:pPr>...</w:pPr><w:pPr>...</w:pPr> by dropping
the first (empty-ish) <w:pPr>, keeping only the second, which contains the
real styling (ImageCaption, etc.). Result opens cleanly in Word.
"""
import re
import sys
import zipfile
import shutil
from pathlib import Path

DOCX_PATH = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("_output/index.docx")

if not DOCX_PATH.exists():
    print(f"[fix_docx] skip: {DOCX_PATH} not found")
    sys.exit(0)

tmp = DOCX_PATH.with_suffix(".tmp.docx")

with zipfile.ZipFile(DOCX_PATH, "r") as zin, zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
    for item in zin.infolist():
        data = zin.read(item.filename)
        if item.filename == "word/document.xml":
            text = data.decode("utf-8")
            pattern = re.compile(r"<w:pPr>(?:(?!</w:pPr>).)*?</w:pPr>\s*(?=<w:pPr>)", re.DOTALL)
            before = len(pattern.findall(text))
            text = pattern.sub("", text)
            data = text.encode("utf-8")
            print(f"[fix_docx] removed {before} redundant <w:pPr> elements")
        zout.writestr(item, data)

shutil.move(tmp, DOCX_PATH)
print(f"[fix_docx] wrote {DOCX_PATH}")

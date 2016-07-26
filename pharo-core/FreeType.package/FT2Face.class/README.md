Do not rearrange these fields!
New fields should go at the end, because the plugin has to know about these indexes.

ByteArray representing a pointer to the malloc'd FT_Face struct:
handle

Copied from the FT_Face struct on creation:
numFaces faceIndex faceFlags styleFlags numGlyphs familyName styleName numFixedSizes availableSizes numCharmaps charmaps

Copied on creation, but only relevant to scalable outlines:
bbox unitsPerEm ascender descender height maxAdvanceWidth maxAdvanceHeight underlinePosition underlineThickness 

Working memory:
glyph -- FT2GlyphSlot, set by loadGlyph or loadChar
size -- the active size, set by activateSize, used by loadGlyph, getKerning, etc.
charmap -- set by setCharmap

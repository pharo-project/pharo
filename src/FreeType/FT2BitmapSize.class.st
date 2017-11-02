"
Do not rearrange these fields!

This structure models the size of a bitmap strike (i.e., a bitmap
instance of the font for a given resolution) in a fixed-size font
face.  It is used for the `availableSizes' field of the
FT2Face structure.

<Fields>
height :: The (vertical) baseline-to-baseline distance in pixels.
It makes most sense to define the height of a bitmap
font in this way.

width  :: The average width of the font (in pixels).  Since the
algorithms to compute this value are different for the
various bitmap formats, it can only give an additional
hint if the `height' value isn't sufficient to select
the proper font.  For monospaced fonts the average width
is the same as the maximum width.

size   :: The point size in 26.6 fractional format this font shall
represent (for a given vertical resolution).

x_ppem :: The horizontal ppem value (in 26.6 fractional format).

y_ppem :: The vertical ppem value (in 26.6 fractional format).
Usually, this is the `nominal' pixel height of the font.

<Note>
The values in this structure are taken from the bitmap font.  If
the font doesn't provide a parameter it is set to zero to indicate
that the information is not available.

The following formula converts from dpi to ppem:

ppem = size * dpi / 72

where `size' is in points.

Windows FNT:
The `size' parameter is not reliable: There exist fonts (e.g.,
app850.fon) which have a wrong size for some subfonts; x_ppem
and y_ppem are thus set equal to pixel width and height given in
in the Windows FNT header.

TrueType embedded bitmaps:
`size', `width', and `height' values are not contained in the
bitmap strike itself.  They are computed from the global font
parameters.

"
Class {
	#name : #FT2BitmapSize,
	#superclass : #Object,
	#instVars : [
		'height',
		'width',
		'size',
		'xPpEm',
		'yPpEm'
	],
	#category : 'FreeType-Base'
}

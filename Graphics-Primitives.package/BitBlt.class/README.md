I represent a block transfer (BLT) of pixels into a rectangle (destX, destY, width, height) of the destinationForm.  The source of pixels may be a similar rectangle (at sourceX, sourceY) in the sourceForm, or a constant color, currently called halftoneForm.  If both are specified, their pixel values are combined with a logical AND function prior to transfer.  In any case, the pixels from the source are combined with those of the destination by as specified by the combinationRule.

The combination rule whose value is 0 through 15 programs the transfer to produce 1 or 0 according to its 4-bit representation as follows:
	8:	if source is 0 and destination is 0
	4:	if source is 0 and destination is 1
	2:	if source is 1 and destination is 0
	1:	if source is 1 and destination is 1.
At each pixel the corresponding bits of the source and destination pixel values determine one of these conditions;  if the combination rule has a 1 in the corresponding bit position, then the new destination value will be 1, otherwise it will be zero.  Forms may be of different depths, see the comment in class Form.

In addition to the original 16 combination rules, this BitBlt supports
	16	fails (to simulate paint bits)
	17	fails (to simulate erase bits)
	18	sourceWord + destinationWord
	19	sourceWord - destinationWord
	20	rgbAdd: sourceWord with: destinationWord.  Sum of color components
	21	rgbSub: sourceWord with: destinationWord.  Difference of color components
	22	OLDrgbDiff: sourceWord with: destinationWord.  Sum of abs of differences in components
	23	OLDtallyIntoMap: destinationWord.  Tallies pixValues into a colorMap
			these old versions don't do bitwise dest clipping.  Use 32 and 33 now.
	24	alphaBlend: sourceWord with: destinationWord.  32-bit source and dest only
	25	pixPaint: sourceWord with: destinationWord.  Wherever the sourceForm is non-zero, it replaces the destination.  Can be used with a 1-bit source color mapped to (0, FFFFFFFF), and a fillColor to fill the dest with that color wherever the source is 1.
	26	pixMask: sourceWord with: destinationWord.  Like pixPaint, but fills with 0.
	27	rgbMax: sourceWord with: destinationWord.  Max of each color component.
	28	rgbMin: sourceWord with: destinationWord.  Min of each color component.
	29	rgbMin: sourceWord bitInvert32 with: destinationWord.  Min with (max-source)
	30	alphaBlendConst: sourceWord with: destinationWord.  alpha is an arg. works in 16 bits.
	31	alphaPaintConst: sourceWord with: destinationWord.  alpha is an arg. works in 16 bits.
	32	rgbDiff: sourceWord with: destinationWord.  Sum of abs of differences in components
	33	tallyIntoMap: destinationWord.  Tallies pixValues into a colorMap
	34	alphaBlendScaled: srcWord with: dstWord. Alpha blend of scaled srcWord and destWord.

The color specified by halftoneForm may be either a Color or a Pattern.   A Color is converted to a pixelValue for the depth of the destinationForm.  If a Pattern, BitBlt will simply interpret its bitmap as an array of Color pixelValues.  BitBlt aligns the first element of this array with the top scanline of the destinationForm, the second with the second, and so on, cycling through the color array as necessary.  Within each scan line the 32-bit value is repeated from left to right across the form.  If the value repeats on pixels boudaries, the effect will be a constant color;  if not, it will produce a halftone that repeats on 32-bit boundaries.

Any transfer specified is further clipped by the specified rectangle (clipX, clipY, clipWidth, clipHeight), and also by the bounds of the source and destination forms.
	To make a small Form repeat and fill a big form, use an InfiniteForm as the source.
	To write on a form and leave with both transparent and opapue areas, use a MaskedForm as the source.

Pixels from a source to a destination whose pixels have a different depth are converted based on the optional colorMap.  If colorMap is nil, then conversion to more bits is done by filling the new high-order bits with zero, and conversion to fewer bits is done by truncating the lost high-order bits.  

The colorMap, if specified, must be a either word array (ie Bitmap) with 2^n elements, where n is the pixel depth of the source, or a fully specified ColorMap which may contain a lookup table (ie Bitmap) and/or four separate masks and shifts which are applied to the pixels. For every source pixel, BitBlt will first perform masking and shifting and then index the lookup table, and select the corresponding pixelValue and mask it to the destination pixel size before storing.
	When blitting from a 32 or 16 bit deep Form to one 8 bits or less, the default is truncation.  This will produce very strange colors, since truncation of the high bits does not produce the nearest encoded color.  Supply a 512 long colorMap, and red, green, and blue will be shifted down to 3 bits each, and mapped.  The message copybits...stdColors will use the best map to the standard colors for destinations of depths 8, 4, 2 and 1.  Two other sized of colorMaps are allowed, 4096 (4 bits per color) and 32786 (five bits per color).
	Normal blits between 16 and 32 bit forms truncates or pads the colors automatically to provide the best preservation of colors.
	Colors can be remapped at the same depth.  Sometimes a Form is in terms of colors that are not the standard colors for this depth, for example in a GIF file.  Convert the Form to a MaskedForm and send colorMap: the list of colors that the picture is in terms of.  MaskedForm will use the colorMap when copying to the display or another Form. (Note also that a Form can be copied to itself, and transformed in the process, if a non-nil colorMap is supplied.)
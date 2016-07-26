I represent the
cairo_text_extents_t
structure type

The cairo_text_extents_t structure stores the extents of a single glyph or a string of glyphs in user-space coordinates. Because text extents are in user-space coordinates, they are mostly, but not entirely, independent of the current transformation matrix. If you call cairo_scale(cr, 2.0, 2.0), text will be drawn twice as big, but the reported text extents will not be doubled. They will change slightly due to hinting (so you can't assume that metrics are independent of the transformation matrix), but otherwise will remain unchanged.

double x_bearing;
	the horizontal distance from the origin to the leftmost part of the glyphs as drawn. Positive if the glyphs lie entirely to the right of the origin.

double y_bearing;
	the vertical distance from the origin to the topmost part of the glyphs as drawn. Positive only if the glyphs lie completely below the origin; will usually be negative.

double width;
	width of the glyphs as drawn

double height;
	height of the glyphs as drawn

double x_advance;
	distance to advance in the X direction after drawing these glyphs

double y_advance;
	distance to advance in the Y direction after drawing these glyphs. Will typically be zero except for vertical text layout as found in East-Asian languages.
path commands
string: aString font: aFont
	"Adds closed paths for the string to the current path. The generated path if filled, achieves an effect similar to that of cairo_show_glyphs()."
	|glyphs metricsProvider|
	metricsProvider := CairoFontMetricsProvider new
		font: aFont.
	glyphs := metricsProvider glyphsOf: aString.
	context setScaledFont: metricsProvider cairoFont.
	self glyphPath: glyphs getHandle size: glyphs size
hint options
setHintMetrics: hintMetrics
"
	void	cairo_font_options_set_hint_metrics (cairo_font_options_t *options,
		cairo_hint_metrics_t hint_metrics);

 Sets the metrics hinting mode for the font options object. This controls whether metrics are quantized to integer values in device units. See the documentation for cairo_hint_metrics_t for full details.

"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self nbCall: #(void cairo_font_options_set_hint_metrics (self,  uint hintMetrics))

 
geometry
fontMetricsProviderFor: aFont 

"aFont glyphRendererOn: surface."

	^ CairoFontMetricsProvider new 
			font: aFont ;
			yourself 
accessing
tabWidthUsingFont: aFont
	"Initialize tab width based on first attempt to use it in text (therefore it will use font metrics of font used in character span, where tab has occured).
	The tab width is then considered same for whole layout, regardless of font size/font metrics used by any other tabs.
	"
	| metricsProvider |

	tabWidth ifNotNil: [ ^ tabWidth ].

	metricsProvider := self fontMetricsProviderFor: aFont.
	"Take arbitrary number and multiply it by 8.. this is how you get the tab width! "
	^ tabWidth := (metricsProvider getGlyphWidth: Character space) * 8.

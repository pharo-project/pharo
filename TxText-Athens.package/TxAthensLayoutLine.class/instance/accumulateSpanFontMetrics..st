accessing
accumulateSpanFontMetrics: span
	"accumulate max font height and ascent for given span"
	| font fontMetricsProvider |

	font := interval start span getAttribute: TxFontAttribute.  
	fontMetricsProvider := layout fontMetricsProviderFor: font.
	self accumulateHeight: fontMetricsProvider fontHeight.
	self accumulateAscent: fontMetricsProvider fontAscent.		
	
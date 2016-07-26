visiting
visitEmbeddedObjectSpan: aSpan

	| font fontMetricsProvider command |

	font := aSpan getAttribute: TxFontAttribute.  
	fontMetricsProvider := layout fontMetricsProviderFor: font.

	command := layout newEmbeddedObjectCommand: aSpan.
	
	"embedded object's height contributes to font ascent, as its bottom is aligned to baseline"
	currentLine accumulateAscent: command height.
	currentLine accumulateHeight: command height + (fontMetricsProvider fontHeight - fontMetricsProvider fontAscent).
	currentLine addWidth: command width.
	
	currentLine addCommand: command.

	^ aSpan next startPosition

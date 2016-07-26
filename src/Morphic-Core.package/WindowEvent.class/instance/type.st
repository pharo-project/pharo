accessing
type
	"This should match the definitions in sq.h"
	^#(
		windowMetricChange
		windowClose
		windowIconise
		windowActivated
		windowPaint
	) at: action ifAbsent: [#windowEventUnknown]
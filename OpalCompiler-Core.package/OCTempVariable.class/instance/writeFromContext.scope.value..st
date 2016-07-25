debugging
writeFromContext: aContext scope: contextScope value: aValue
	
	"offset := contextScope outerOffsetTo: scope.
	contextForRead := aContext.
	offset timesRepeat: [contextForRead := contextForRead outerContext].
	"^aContext tempAt: self indexFromIR put: aValue
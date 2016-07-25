actions
textExtent: newExtent
	"If autoFit is on then override to false for the duration of the extent call."
	
	textMorph ifNil: [^self].
	textMorph overrideExtent: newExtent
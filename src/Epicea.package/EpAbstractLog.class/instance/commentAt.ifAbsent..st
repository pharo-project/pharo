accessing
commentAt: anEntry ifAbsent: aBlock 
	"Answer the String comment corresponding to anEntry, or evaluate aBlock if absent."

	^ self subclassResponsibility
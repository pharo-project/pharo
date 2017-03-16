accessing
commentAt: anEntry ifPresent: aBlock 
	"Answer the String comment corresponding to anEntry, and evaluate aBlock with it."

	| comment |
	comment := self commentAt: anEntry ifAbsent: [ ^self ].
	^ aBlock value: comment.
as yet unclassified
gripLayoutFrame
	"Answer the layout frame dependinbg on our edge."
	
	self edgeName == #top ifTrue: [^self topLayoutFrame].
	self edgeName == #bottom ifTrue: [^self bottomLayoutFrame].
	self edgeName == #left ifTrue: [^self leftLayoutFrame].
	^self rightLayoutFrame
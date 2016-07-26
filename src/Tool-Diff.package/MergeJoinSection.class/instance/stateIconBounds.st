as yet unclassified
stateIconBounds
	"Answer the bounds of the state icon."
	
	|i|
	i := self stateIcon ifNil: [^nil].
	^self shape bounds center - (i extent // 2)
		extent: i extent
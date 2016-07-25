operation
tryMergeWithNext
	"Applicable only to character spans.
	
	Merge with next span, if it has same attributes"
	characters size isZero ifTrue: [ ^ self delete ].
	
	[ next isCharacterSpan ] whileTrue: [
	
		next style = self style ifTrue: [
			characters := characters , next characters.
			next delete
		] ifFalse: [ ^ self ]
	]

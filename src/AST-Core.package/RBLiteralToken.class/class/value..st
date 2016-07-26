instance creation
value: anObject 
	| literal |
	literal := anObject class == Array 
				ifTrue: [anObject collect: [:each | self value: each]]
				ifFalse: [anObject].
	^self 
		value: literal
		start: nil
		stop: nil
reading
commentStampFor: aRingClass
	^ aRingClass hasComment
		ifTrue: [ aRingClass comment stamp ]
		ifFalse: [ nil ]
reading
commentFor: aRingClass

	^ aRingClass hasComment
		ifTrue: [ aRingClass comment content asString ]
		ifFalse: [ '' ]
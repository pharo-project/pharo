Extension { #name : #ScrollBar }

{ #category : #'*glamour-morphic-widgets' }
ScrollBar >> glmAnimateValue: aNumber duration: anInteger [
"	aNumber = value ifTrue: [^ self ]."
	anInteger <= 0 
		ifTrue: [ self setValue: aNumber ]
		ifFalse: [
			| startTime start end |
			startTime := Time millisecondClockValue.
			start := value "roundTo: scrollDelta".
			[ | delta |
			[ (delta := Time millisecondClockValue - startTime) < anInteger ] whileTrue: [
				self setValue: (aNumber - start) * (delta / anInteger) + start.
				Processor yield ].
				self setValue: aNumber ]
					fork ]
]

{ #category : #'*glamour-morphic-widgets' }
ScrollBar >> setOptimalResizing [
	self hResizing: #rigid.
	self vResizing: #rigid
]

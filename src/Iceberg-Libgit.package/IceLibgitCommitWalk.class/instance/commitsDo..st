walking
commitsDo: aBlock 
	self maxNumber 
		ifNil: [ super commitsDo: aBlock ]
		ifNotNil: [ | count |
			count := 0.
			super commitsDo: [ :commit |
				aBlock value: commit.
				count := count + 1.
				count == self maxNumber ifTrue: [ ^ self ]
			]
		]

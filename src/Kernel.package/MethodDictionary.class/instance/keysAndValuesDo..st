enumeration
keysAndValuesDo: aBlock 
	"Enumerate the receiver with all the keys and values passed to the block"
	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil:
			[ :key | aBlock value: key value: (array at: i)]
		]
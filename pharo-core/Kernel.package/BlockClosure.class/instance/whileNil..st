controlling
whileNil: aBlock 
	"Unlike #whileTrue/False: this is not compiled inline."
	^ [self value isNil] whileTrue: [aBlock value]
	
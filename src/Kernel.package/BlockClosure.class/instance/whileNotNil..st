controlling
whileNotNil: aBlock 
	"Unlike #whileTrue/False: this is not compiled inline."
	^ [self value notNil] whileTrue: [aBlock value]
	
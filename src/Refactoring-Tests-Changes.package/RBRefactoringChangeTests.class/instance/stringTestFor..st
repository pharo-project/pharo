utilities
stringTestFor: aChange
	self assert: (aChange name isString and: [ aChange name notEmpty ]).
	self assert: (aChange printString isString and: [ aChange printString notEmpty ]).
	self assert: (aChange changeString isString and: [ aChange changeString notEmpty ]).
	self assert: (aChange displayString isString and: [ aChange displayString notEmpty ])
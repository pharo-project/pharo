accessing
methodClass: aClass
	"set the class binding in the last literal to aClass"
	^self numLiterals > 0
		ifTrue: [ self literalAt: self numLiterals put: aClass binding ]	
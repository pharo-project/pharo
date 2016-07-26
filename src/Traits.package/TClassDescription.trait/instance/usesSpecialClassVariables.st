slots
usesSpecialClassVariables

	^self classVariables anySatisfy: [ :each | each isSpecial ]
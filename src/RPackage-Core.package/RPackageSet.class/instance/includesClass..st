testing
includesClass: aClass
	^self packages anySatisfy: [ :each | each includesClass: aClass ]
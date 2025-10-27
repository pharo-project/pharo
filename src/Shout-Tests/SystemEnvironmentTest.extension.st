Extension { #name : 'SystemEnvironmentTest' }

{ #category : '*Shout-Tests' }
SystemEnvironmentTest >> testHasBindingThatBeginsWith [

	super testHasBindingThatBeginsWith.
	self assert: (Smalltalk globals hasBindingThatBeginsWith: 'Obje').
	self deny: (Smalltalk globals hasBindingThatBeginsWith: 'NOTHEREIope')
]

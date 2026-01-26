Extension { #name : 'OCScopeTest' }

{ #category : '*Shout-Tests' }
OCScopeTest >> testHasBindingThatBeginsWithClassVar [
	self assert: (thisContext astScope hasBindingThatBeginsWith: 'MyClass').
	self deny: (thisContext astScope hasBindingThatBeginsWith: 'NonExMYCL')
]

{ #category : '*Shout-Tests' }
OCScopeTest >> testHasBindingThatBeginsWithGlobal [
	self assert: (thisContext astScope hasBindingThatBeginsWith: 'Obj').
	self deny: (thisContext astScope hasBindingThatBeginsWith: 'OBj')
]

{ #category : '*Shout-Tests' }
OCScopeTest >> testHasBindingThatBeginsWithSlot [
	self assert: (thisContext astScope hasBindingThatBeginsWith: 'ivarFo').
	self deny: (thisContext astScope hasBindingThatBeginsWith: 'ivarForDoesNotExist').

	"super and self, too"
	self assert: (thisContext astScope hasBindingThatBeginsWith: 'sel').
	self assert: (thisContext astScope hasBindingThatBeginsWith: 'sup')
]

{ #category : '*Shout-Tests' }
OCScopeTest >> testHasBindingThatBeginsWithTemp [
	| tempForTesting |
	tempForTesting := 'only',  'forTest'.
	self assert: (thisContext astScope hasBindingThatBeginsWith: 'tempFor').
	self deny: (thisContext astScope hasBindingThatBeginsWith: 'TempForTest').

	"thisContext is known, too"
	self assert: (thisContext astScope hasBindingThatBeginsWith: 'thisC').
	^tempForTesting
]

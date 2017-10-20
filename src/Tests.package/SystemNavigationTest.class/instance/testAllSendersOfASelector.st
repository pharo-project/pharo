testing
testAllSendersOfASelector
	| senders selector class otherClass callers |
	selector := ('aMethod', 'WithSenders') asSymbol.
	
	class := self classFactory newClass.
	class compileSilently: selector asString, ' ^ self'.
	class compileSilently: 'anotherMethod ^self ', selector asString.
	otherClass := self classFactory newClass.
	otherClass compileSilently: 'yetAnotherMethod ^self ', selector asString.
	senders := self systemNavigationToTest allSendersOf: selector.
	self assert: senders size = 2.
	callers := senders collect: [ :methodRef | methodRef selector].
	self assert: (callers includesAll: #(#anotherMethod #yetAnotherMethod))

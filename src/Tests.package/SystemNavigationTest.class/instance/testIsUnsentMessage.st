testing
testIsUnsentMessage
	| class |
	class := self classFactory newClass.
	class compileSilently: 'messageNeverSentInTheSystemXXXXThisIsForTest ^self'.
	self assert: (self systemNavigationToTest isUnsentMessage: class selectors anyOne)
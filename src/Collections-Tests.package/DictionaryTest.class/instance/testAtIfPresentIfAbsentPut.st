tests - DictionaryIndexAccessing
testAtIfPresentIfAbsentPut
	| dico actual |
	dico := Dictionary new.
	dico at: #ok put: 7.
	actual := dico at: #o ifPresent: [ #testNotOk ] ifAbsentPut: [ #testOk ].
	self assert: actual equals: #testOk.
	self assert: (dico at: #o) equals: #testOk.
	actual := dico at: #ok ifPresent: [ #cool ] ifAbsent: [ #notCool ].
	self assert: actual equals: #cool.
	self assert: (dico at: #o) equals: #testOk
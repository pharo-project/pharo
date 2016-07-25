tests
testallSuperclassesIncluding
	"self debug: #testallSuperclassesIncluding"
	
	|cls |
	cls := ArrayedCollection allSuperclassesIncluding: Collection.
	self deny: (cls includes: ArrayedCollection).
	self deny: (cls includes: Object). 
	self assert: (cls includes: Collection). 
	self assert: (cls includes: SequenceableCollection). 
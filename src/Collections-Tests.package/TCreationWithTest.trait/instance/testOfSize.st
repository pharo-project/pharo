tests - creation
testOfSize
	"self debug: #testOfSize"
	
	| aCol |
	aCol := self collectionClass ofSize: 3.
	self assert: (aCol size = 3).

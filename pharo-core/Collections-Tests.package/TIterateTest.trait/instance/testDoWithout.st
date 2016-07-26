tests - iterating
testDoWithout
	"self debug: #testDoWithout"
	
	| res element collection |
	collection := self collectionWithoutNilElements .	
	res := OrderedCollection new.  
	element := self collectionWithoutNilElements anyOne .
	collection  do: [:each | res add: each] without: element  .
	" verifying result :"
	self assert: res size = (collection  size - (collection  occurrencesOf: element)).
	res do: [:each | self assert: (collection occurrencesOf: each) = ( res occurrencesOf: each ) ].
	
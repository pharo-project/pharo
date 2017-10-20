tests - creation
testWithWithWithWithWith
	"self debug: #testWithWithWithWithWith"
	
	| aCol collection |
	collection := self collectionMoreThan5Elements asOrderedCollection copyFrom: 1 to: 5 .
	aCol := self collectionClass with: (collection at:1) with: ( collection at:2 ) with: ( collection at: 3) with: (collection at: 4 ) with: ( collection at: 5 ).

	1 to: 3 do: [ :i | self assert: ( aCol occurrencesOf: ( collection at: i ) ) = ( collection occurrencesOf: ( collection at: i ) ) ].
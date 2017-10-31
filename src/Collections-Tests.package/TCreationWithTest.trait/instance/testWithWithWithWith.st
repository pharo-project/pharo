tests - creation
testWithWithWithWith
	"self debug: #testWithWithWithWith"
	
	| aCol collection |
	collection := self collectionMoreThan5Elements asOrderedCollection copyFrom: 1 to: 4.
	aCol := self collectionClass with: (collection at:1) with: ( collection at:2 ) with: ( collection at: 3) with: ( collection at: 4).

	1 to: 3 do: [ :i | self assert: ( aCol occurrencesOf: ( collection at: i ) ) = ( collection occurrencesOf: ( collection at: i ) ) ].
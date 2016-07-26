tests - replacing
testReplaceFromToWith
	| result  collection replacementCollec firstInd secondInd |
	collection := self nonEmpty .
	replacementCollec := self replacementCollectionSameSize .
	firstInd := self firstIndex .
	secondInd := self secondIndex .
	result := collection  copy.
	result replaceFrom: firstInd  to: secondInd  with: replacementCollec .
	
	"verify content of 'result' : "
	"first part of 'result'' : '"
	
	1 to: ( firstInd - 1 ) do: [ :i | self assert: (collection  at:i ) = ( result at: i ) ].
	
	" middle part containing replacementCollection : "
	
	( firstInd ) to: ( firstInd  + replacementCollec size - 1 ) do: 
		[ :i |
		self assert: ( result at: i ) = ( replacementCollec  at: ( i - firstInd  +1 ) ) 
		].
	
	" end part :"
	( firstInd  + replacementCollec   size) to: (result size) do:
		[:i|
		self assert: ( result at: i ) = ( collection at: ( secondInd  + 1 - ( firstInd + replacementCollec size ) + i ) ) ].
	
	
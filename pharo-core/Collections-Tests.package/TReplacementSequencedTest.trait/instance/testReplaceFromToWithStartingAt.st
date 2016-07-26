tests - replacing
testReplaceFromToWithStartingAt
	| result  repStart collection replacementCollec firstInd secondInd |
	collection := self nonEmpty .
	result := collection copy.
	replacementCollec := self replacementCollectionSameSize .
	firstInd := self firstIndex .
	secondInd := self secondIndex .
	repStart := replacementCollec  size - ( secondInd  - firstInd   + 1 ) + 1.
	result replaceFrom: firstInd  to: secondInd with: replacementCollec  startingAt: repStart   .
	
	"verify content of 'result' : "
	"first part of 'result'' : '" 
	
	1 to: ( firstInd  - 1 ) do: [ :i | self assert: ( collection  at:i ) = ( result at: i ) ].
	
	" middle part containing replacementCollection : "
	
	( firstInd ) to: ( replacementCollec   size - repStart +1 ) do: 
		[:i|
		self assert: (result at: i)=( replacementCollec   at: ( repStart  + ( i  - firstInd  ) ) ) ].
	
	" end part :"
	( firstInd  + replacementCollec   size ) to: ( result size ) do:
		[ :i |
		self assert: ( result at: i ) = ( collection  at: ( secondInd  + 1 - ( firstInd  + replacementCollec   size ) + i ) ) ].
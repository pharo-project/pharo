tests - copying with replacement
testCopyReplaceAllWith1Occurence
	| result  firstIndexesOfOccurrence index endPartIndexResult endPartIndexCollection |
	
	result := self collectionWith1TimeSubcollection  copyReplaceAll: self oldSubCollection with: self replacementCollection .
	
	"detecting indexes of olSubCollection"
	firstIndexesOfOccurrence  := self firstIndexesOf: self oldSubCollection in: self collectionWith1TimeSubcollection .
	index:= firstIndexesOfOccurrence at: 1.
	
	"verify content of 'result' : "
	"first part of 'result'' : '"

	1 to: (index -1) do: 
		[
		:i |  
		self assert: (self collectionWith1TimeSubcollection  at:i)=(result at: i)
		].

	" middle part containing replacementCollection : "
	
	index to: (index + self replacementCollection size-1) do: 
		[
		:i |
		self assert: ( result at: i )=(self replacementCollection at: ( i - index + 1 ))
		].
	
	" end part :"
	
	endPartIndexResult :=  index + self replacementCollection  size .
	endPartIndexCollection :=   index + self oldSubCollection size  .
	
	1 to: (result size - endPartIndexResult - 1 ) do:
		[ 
		:i |
		self assert: (result at: ( endPartIndexResult + i - 1 ) ) = (self collectionWith1TimeSubcollection  at: ( endPartIndexCollection + i - 1 ) ).
		].
	
	
	
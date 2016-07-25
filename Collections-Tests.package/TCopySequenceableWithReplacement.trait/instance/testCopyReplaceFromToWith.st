tests - copying with replacement
testCopyReplaceFromToWith
	| result  indexOfSubcollection lastIndexOfOldSubcollection lastIndexOfReplacementCollection |
	
	indexOfSubcollection := (self firstIndexesOf: self oldSubCollection  in:  self collectionWith1TimeSubcollection) at: 1. 
	lastIndexOfOldSubcollection := indexOfSubcollection + self oldSubCollection size -1.
	lastIndexOfReplacementCollection := indexOfSubcollection + self replacementCollection  size -1.
	
	result := self collectionWith1TimeSubcollection  copyReplaceFrom: indexOfSubcollection  to: lastIndexOfOldSubcollection   with: self replacementCollection .
	
	"verify content of 'result' : "
	"first part of 'result'  "
	
	1 to: (indexOfSubcollection  - 1) do: 
		[ 
		:i | 
		self assert: (self collectionWith1TimeSubcollection  at:i) = (result at: i)
		].
	
	" middle part containing replacementCollection : "
	
	(indexOfSubcollection ) to: ( lastIndexOfReplacementCollection  ) do: 
		[
		:i |
		self assert: (result at: i)=(self replacementCollection at: (i - indexOfSubcollection +1))
		].
	
	" end part :"
	1 to: (result size - lastIndexOfReplacementCollection   ) do:
		[ 
		:i |
		self assert: (result at: ( lastIndexOfReplacementCollection  + i  ) ) = (self collectionWith1TimeSubcollection  at: ( lastIndexOfOldSubcollection  + i  ) ).
		].
	
	
	

	
	
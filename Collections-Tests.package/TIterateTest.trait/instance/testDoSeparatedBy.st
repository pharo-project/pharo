tests - iterating
testDoSeparatedBy
	| string expectedString beforeFirst |
	
	string := ''.
	self collectionWithoutNilElements  
		do: [ :each | string := string , each asString ]
		separatedBy: [ string := string , '|' ].
		
	expectedString := ''.
	beforeFirst := true.
	self collectionWithoutNilElements  do: 
		[ :each | 
		beforeFirst = true 
			ifTrue: [ beforeFirst := false ]
			ifFalse: [ expectedString := expectedString , '|' ].
		expectedString := expectedString , each asString ].
	self assert: expectedString = string
tests - iterating
testSumNumbers
	|result|
	result:= self collectionWithoutNilElements 
		sumNumbers: [ :ele | ele notNil ifTrue: [ 1 ] ifFalse: [ 0 ]].
	
	self assert: self collectionWithoutNilElements size = result
tests - iterating
testInjectInto
	|result|
	result:= self collectionWithoutNilElements 
		inject: 0
		into: [:inj :ele | ele notNil ifTrue: [ inj + 1 ]].
	
	self assert: self collectionWithoutNilElements size = result .
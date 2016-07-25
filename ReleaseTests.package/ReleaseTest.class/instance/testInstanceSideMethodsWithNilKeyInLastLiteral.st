testing
testInstanceSideMethodsWithNilKeyInLastLiteral
	| instanceSideMethodsWithNilKeyInLastLiteral |
	
	instanceSideMethodsWithNilKeyInLastLiteral := SystemNavigation new instanceSideMethodsWithNilKeyInLastLiteral.
	
	self 
		assert: instanceSideMethodsWithNilKeyInLastLiteral isEmpty
		description: [ String streamContents: [ :s|
			s 
				nextPutAll: 'Found methods with nil as last literal: ';
				print: instanceSideMethodsWithNilKeyInLastLiteral ]].
	
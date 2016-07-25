running
tearDownInstanceVariables
	self class withAllSuperclasses do: [ :class |
		class = TestCase ifTrue: [ ^ self ].
		
		class instVarNames do: [ :varName |
			self 
				instVarNamed: varName
				put: nil ] ]
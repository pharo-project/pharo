comparing
= aCompiledMethod
	"Answer whether the receiver implements the same code as aCompiledMethod."

	self == aCompiledMethod
		ifTrue: [ ^ true ].
	self class = aCompiledMethod class
		ifFalse: [ ^ false ].
	self size = aCompiledMethod size
		ifFalse: [ ^ false ].
	self header = aCompiledMethod header
		ifFalse: [ ^ false ].
	self initialPC to: self endPC do: [ :i | 
		(self at: i) = (aCompiledMethod at: i)
			ifFalse: [ ^ false ] ].
	(self sameLiteralsAs: aCompiledMethod)
		ifFalse: [ ^ false ].
	^ true
enumerating
withContentsDo: aBlock
	aBlock value: self.
	self containerTree glamourPresentation allowsRecursiveSearch ifTrue: [  
		self contents do: [:each | each withContentsDo: aBlock ] ]
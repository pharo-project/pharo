widget category
selectedCategory: protocol
	self okToChange
		ifTrue: [ 
			self resetMethodsList.
			self model
				package: self selectedPackage
				class: self selectedClass
				protocol: protocol.
			self resetMethodsListSelection.
			protocol
				ifNil: [ acceptor := ClassOrMethodDefinitionAcceptor model: self ]
				ifNotNil: [ 
					acceptor := MethodDefinitionAcceptor model: self.
					self giveFocusTo: categoryWidget ].
			categoryWidget changed: #selectedCategoryIndex.
			^ true ].
	^ false
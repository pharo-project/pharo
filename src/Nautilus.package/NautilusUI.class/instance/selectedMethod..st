widget method
selectedMethod: aMethod 

	self okToChange
		ifTrue: [
			self forceSelectedMethod: aMethod.
			self setWindowTitle.
			self highlightCategory: aMethod.
			aMethod 
				ifNil: [ 
					acceptor := self selectedCategory 
						ifNil: [ ClassOrMethodDefinitionAcceptor model: self ]
						ifNotNil:[ MethodDefinitionAcceptor model: self ]]
				ifNotNil: [ 
					acceptor := MethodDefinitionAcceptor model: self.
					self giveFocusTo: methodWidget ].
			^ true].
	^false 
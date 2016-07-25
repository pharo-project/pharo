accessing
showInstance: aBoolean

	self okToChangeSourceAndComment
		ifTrue:[
			self model showInstance: aBoolean.
			self resetClassesListSelection.
			self selectedClass 
				ifNotNil: [:class |
					aBoolean 
						ifTrue: [self selectedClass: class theNonMetaClass.
								acceptor := ClassOrMethodDefinitionAcceptor model: self ]
						ifFalse: [self selectedClass: class theMetaClass.
								acceptor := ClassDefinitionAcceptor model: self ]].
			self updateCategoryAndMethodList.
			self changed: #instanceButtonState.
			self changed: #instanceButtonLabel ]
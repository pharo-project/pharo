bindings
appendVariableBinding: anAssociationList

	anAssociationList ifNotNil: [
		anAssociationList do: [ :anAssociation | 
			self variableBindings add: 
				((anAssociation class = WorkspaceVariable) 
					ifTrue: [ anAssociation ] 
					ifFalse: [ (WorkspaceVariable key: anAssociation key value: anAssociation value) ]) ] ]
bindings
appendVariableBinding: anAssociationList

	anAssociationList ifNotNil: [
		anAssociationList do: [ :anAssociation | 
			self variableBindings add: (WorkspaceVariable key: anAssociation key value: anAssociation value) ] ]
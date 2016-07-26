private
workspaceBindingsStrategy

	^ [ :currentBindings :aString |
		(currentBindings includesKey: aString)
			ifFalse: [currentBindings add: (WorkspaceVariable key: aString asSymbol) ].
		currentBindings associationAt: aString]
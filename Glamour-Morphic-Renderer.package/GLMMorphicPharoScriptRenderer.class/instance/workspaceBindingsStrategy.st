private
workspaceBindingsStrategy

	^ [ :currentBindings :aString :aTextModel |
		(aTextModel doItReceiver class allInstVarNames includes: aString) 
			ifTrue: [ nil ]
			ifFalse: [  	
				(currentBindings includesKey: aString)
					ifFalse: [currentBindings add: (WorkspaceVariable key: aString asSymbol) ].
				currentBindings associationAt: aString] ]
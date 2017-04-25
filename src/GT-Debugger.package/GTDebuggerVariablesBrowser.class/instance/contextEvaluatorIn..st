private building
contextEvaluatorIn: aComposite

	aComposite pharoScript
		doItReceiver: [ self debugger selectedContext receiver ];
		doItContext: [ self debugger selectedContext ];
		smalltalkClass: [ self debugger selectedContext receiver class ];
		variableBindings: [ self debugger selectedContext gtDebuggerDoItBindings ];
		display: [ 
			String streamContents: [ :stream | 
				stream 
					nextPutAll: (GTSnippets snippetAt: self debugger selectedContext receiver class)] ];
		onChangeOfPort: #text act: [ :textPresentation |
				GTSnippets 
					snippetAt: self debugger selectedContext receiver class 
					put: (textPresentation text asString).
				 textPresentation clearUserEdits ];
		installDefaultSelectionActions
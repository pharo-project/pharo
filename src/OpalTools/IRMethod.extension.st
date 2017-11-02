Extension { #name : #IRMethod }

{ #category : #'*OpalTools' }
IRMethod >> gtInspectorBytecodeIn: composite [
	<gtInspectorPresentationOrder: 40> 
	composite text 
		title: 'Bytecode';
		display: [ self compiledMethod symbolic ]
]

{ #category : #'*OpalTools' }
IRMethod >> gtInspectorIrIn: composite [
	<gtInspectorPresentationOrder: 35> 
	composite text 
		title: 'Symbolic';
		display: [ :anObject || string |
				string := anObject longPrintString.
				string copyFrom: 2 to: string size ]
]

{ #category : #'*OpalTools' }
IRMethod >> gtInspectorSourceCodeIn: composite [ 
	<gtInspectorPresentationOrder: 40>
	self sourceNode gtInspectorSourceCodeIn: composite
]

{ #category : #'*OpalTools' }
IRMethod >> gtInspectorTreeIn: composite [
	<gtInspectorPresentationOrder: 30> 
	^ (GTIRBrowser new treeIn: composite)
		title: 'IR'
]

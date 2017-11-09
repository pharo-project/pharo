Extension { #name : #IRMethod }

{ #category : #'*OpalCompiler-Tools' }
IRMethod >> gtInspectorBytecodeIn: composite [
	<gtInspectorPresentationOrder: 40> 
	composite text 
		title: 'Bytecode';
		display: [ self compiledMethod symbolic ]
]

{ #category : #'*OpalCompiler-Tools' }
IRMethod >> gtInspectorIrIn: composite [
	<gtInspectorPresentationOrder: 35> 
	composite text 
		title: 'Symbolic';
		display: [ :anObject || string |
				string := anObject longPrintString.
				string copyFrom: 2 to: string size ]
]

{ #category : #'*OpalCompiler-Tools' }
IRMethod >> gtInspectorSourceCodeIn: composite [ 
	<gtInspectorPresentationOrder: 40>
	self sourceNode gtInspectorSourceCodeIn: composite
]

{ #category : #'*OpalCompiler-Tools' }
IRMethod >> gtInspectorTreeIn: composite [
	<gtInspectorPresentationOrder: 30> 
	^ (GTIRBrowser new treeIn: composite)
		title: 'IR'
]

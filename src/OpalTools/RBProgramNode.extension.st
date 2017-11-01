Extension { #name : #RBProgramNode }

{ #category : #'*OpalTools' }
RBProgramNode >> gtInspectorSymbolicBytecodeIn: composite [
	<gtInspectorPresentationOrder: 30> 
	^ (GTOpalScopeBrowser new treeIn: composite)
		title: 'Scopes'
]

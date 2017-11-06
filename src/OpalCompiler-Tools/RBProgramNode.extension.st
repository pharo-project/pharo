Extension { #name : #RBProgramNode }

{ #category : #'*OpalCompiler-Tools' }
RBProgramNode >> gtInspectorSymbolicBytecodeIn: composite [
	<gtInspectorPresentationOrder: 30> 
	^ (GTOpalScopeBrowser new treeIn: composite)
		title: 'Scopes'
]

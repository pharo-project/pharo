Class {
	#name : 'ClyNotebookPageRecyclerTest',
	#superclass : 'ClyBrowserContextAbstractTest',
	#category : 'Calypso-Browser-Tests',
	#package : 'Calypso-Browser-Tests'
}

{ #category : 'tests' }
ClyNotebookPageRecyclerTest >> testDoesNotRecycleToolWhenNotValidInContext [

	| recycler tools |
	tools := browser tabManager tools select: [ :tool | tool class = ClyRichTextClassCommentEditorToolMorph ].
	recycler := ClyNotebookPageRecycler on: tools contexts: { self methodContext }.
	
	self assert: recycler toolsToKeep isEmpty.
]

{ #category : 'tests' }
ClyNotebookPageRecyclerTest >> testRecycleToolWhenValidInContext [
	
	| recycler tools |
	tools := browser tabManager tools select: [ :tool | tool class = ClyRichTextClassCommentEditorToolMorph ].
	
	recycler := ClyNotebookPageRecycler on: tools contexts: { self classContext }.
	
	self assert: recycler toolsToKeep size equals: 1.		
	self assert: recycler toolsToKeep anyOne class equals: ClyRichTextClassCommentEditorToolMorph.
]

{ #category : 'tests' }
ClyNotebookPageRecyclerTest >> testTargetedToolsWhenManyContext [

	| recycler tools |
	recycler := ClyNotebookPageRecycler on: nil.
	
	recycler setTargetedToolsForContexts: { self methodContext . self classContext }.
	
	self assert: recycler targetedTools size equals: 4.
	tools := recycler targetedTools collect: [ :targetTool | targetTool activation toolClass ].
	self assertCollection: tools hasSameElements: { ClyMethodCodeEditorToolMorph . ClyClassDefinitionEditorToolMorph . ClyRichTextClassCommentEditorToolMorph . ClyTestSetUpEditorToolMorph }.

]

{ #category : 'tests' }
ClyNotebookPageRecyclerTest >> testTargetedToolsWhenOneContext [

	| recycler |
	recycler := ClyNotebookPageRecycler on: nil.
	
	recycler setTargetedToolsForContexts: { self methodContext }.
	
	self assert: recycler targetedTools size equals: 1.
	self assert: recycler targetedTools anyOne activation toolClass equals: ClyMethodCodeEditorToolMorph.

]

{ #category : 'tests' }
ClyNotebookPageRecyclerTest >> testToolsToInstall [

	| recycler |
	recycler := ClyNotebookPageRecycler 
		on: #()
		contexts: { self methodContext }.
	
	self assert: recycler toolsToInstall size equals: 1.
	self assert: recycler toolsToInstall anyOne activation toolClass equals: ClyMethodCodeEditorToolMorph.
]

{ #category : 'tests' }
ClyNotebookPageRecyclerTest >> testToolsToKeepWhenManyContext [

	| recycler tools |
	recycler := ClyNotebookPageRecycler 
		on: browser tabManager tools 
		contexts: { self methodContext . self classContext }.
	
	self assert: recycler toolsToKeep size equals: 4.
	tools := recycler toolsToKeep collect: [ :tool | tool class ].
	self assertCollection: tools hasSameElements: { ClyMethodCodeEditorToolMorph . ClyClassDefinitionEditorToolMorph . ClyRichTextClassCommentEditorToolMorph . ClyTestSetUpEditorToolMorph }.

]

{ #category : 'tests' }
ClyNotebookPageRecyclerTest >> testToolsToKeepWhenOneContext [

	| recycler |
	recycler := ClyNotebookPageRecycler 
		on: browser tabManager tools
		contexts: { self methodContext }.
	
	self assert: recycler toolsToKeep size equals: 1.
	self assert: recycler toolsToKeep anyOne class equals: ClyMethodCodeEditorToolMorph.

]

{ #category : 'tests' }
ClyNotebookPageRecyclerTest >> testToolsToRemoveWhenManyToolsToRemove [

	| recycler |
	recycler := ClyNotebookPageRecycler 
		on: browser tabManager tools
		contexts: { self methodContext }.
	
	self assert: recycler toolsToRemove size equals: 5.
	self assert: recycler toolsToKeep size equals: 1.
]

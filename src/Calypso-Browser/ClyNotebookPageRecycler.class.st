"
I allow to recycle existing tools in a SystemBrowser (instances of `ClyBrowserToolMorph`) when the context changes.
It avoids to rebuild all tools when they are still valid in the new context.

I need a list of current tools and the list of new contexts to compute the list of tools to keep, to remove and the new one to install. For this purpose, I build a list of targeted tools and I try to find a tool matching the target tool in the existing tools.
I use `ClyTargetTool` to do the matching.
"
Class {
	#name : #ClyNotebookPageRecycler,
	#superclass : #Object,
	#instVars : [
		#tools,
		#targetedTools,
		#isMatched
	],
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #'instance creation' }
ClyNotebookPageRecycler class >> on: toolList [

	^ self new
		tools: toolList;
		yourself
]

{ #category : #'instance creation' }
ClyNotebookPageRecycler class >> on: toolList contexts: aListOfClyBrowserContext [

	^ (self on: toolList)
		setTargetedToolsForContexts: aListOfClyBrowserContext;
		yourself
]

{ #category : #private }
ClyNotebookPageRecycler >> activationStrategiesFor: aClyBrowserContext [

	^ ClyTabActivationStrategyAnnotation activeInstancesInContext: aClyBrowserContext
]

{ #category : #private }
ClyNotebookPageRecycler >> ensureMatched [

	isMatched ifTrue: [ ^ self ].
	self matchTools.
	isMatched := true
]

{ #category : #private }
ClyNotebookPageRecycler >> matchTools [

	self targetedTools do: [ :targetTool | targetTool match: tools ]
]

{ #category : #initialization }
ClyNotebookPageRecycler >> setTargetedToolsForContexts: aListOfClyBrowserContext [

	targetedTools := aListOfClyBrowserContext flatCollect: [ :aClyBrowserContext |
			self targetedToolsFor: aClyBrowserContext ].
	isMatched := false
]

{ #category : #accessing }
ClyNotebookPageRecycler >> targetedTools [

	^ targetedTools
]

{ #category : #private }
ClyNotebookPageRecycler >> targetedToolsFor: aClyBrowserContext [

	^ (self activationStrategiesFor: aClyBrowserContext)
		collect: [ :strategy | ClyTargetTool forActivation: strategy context: aClyBrowserContext]
]

{ #category : #accessing }
ClyNotebookPageRecycler >> tools: toolList [

	tools := toolList
]

{ #category : #querying }
ClyNotebookPageRecycler >> toolsToInstall [

	self ensureMatched.

	^ self targetedTools
		reject: [ :tool | tool isMatched ]
]

{ #category : #accessing }
ClyNotebookPageRecycler >> toolsToKeep [

	self ensureMatched.

	^ self targetedTools
		select: [ :tool | tool isMatched ]
		thenCollect: [ :tool | tool matchedTool ]
]

{ #category : #querying }
ClyNotebookPageRecycler >> toolsToRemove [

	^ tools difference: self toolsToKeep
]

Extension { #name : 'ClyClassDefinitionEditorToolMorph' }

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyClassDefinitionEditorToolMorph >> criticAnalysisScope [

	^ ClyClassScope of: self editingClass in: self browser navigationEnvironment
]

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyClassDefinitionEditorToolMorph >> decorateByCritic: aCriticTool [
]

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyClassDefinitionEditorToolMorph >> supportsCriticDecoration [
	^true
]

Extension { #name : #ClyClassDefinitionEditorToolMorph }

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyClassDefinitionEditorToolMorph >> criticAnalysisScope [
	^ ClyClassScope of: editingClass in: browser navigationEnvironment
]

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyClassDefinitionEditorToolMorph >> decorateByCritic: aCriticTool [

]

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyClassDefinitionEditorToolMorph >> supportsCriticDecoration [
	^true
]

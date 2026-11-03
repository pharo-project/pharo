Extension { #name : 'ClyMethodCodeEditorToolMorph' }

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyMethodCodeEditorToolMorph >> criticAnalysisScope [

	^ ClyMethodScope of: self editingMethod in: self browser navigationEnvironment
]

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyMethodCodeEditorToolMorph >> decorateByCritic: aCriticTool [

	| segments |
	(self textMorph segments select: [ :each | each class = ClyCriticalTextSegmentMorph ]) do: [ :each | each delete ]. "We should not remove while iterating"

	segments := aCriticTool buildCriticalTextSegmentsFor: self editingMethod.
	segments do: [ :each | self textMorph addSegment: each ]
]

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyMethodCodeEditorToolMorph >> supportsCriticDecoration [
	^true
]

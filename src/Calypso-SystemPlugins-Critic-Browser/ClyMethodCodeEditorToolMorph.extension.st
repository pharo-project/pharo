Extension { #name : #ClyMethodCodeEditorToolMorph }

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyMethodCodeEditorToolMorph >> criticAnalysisScope [
	^ ClyMethodScope of: editingMethod in: browser navigationEnvironment
]

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyMethodCodeEditorToolMorph >> decorateByCritic: aCriticTool [
	| segments |
	(textMorph segments select: [ :each | each class = ClyCriticalTextSegmentMorph ])
		do: [ :each | each delete ]. "We should not remove while iterating"
	
	segments := aCriticTool buildCriticalTextSegmentsFor: editingMethod.	
	segments do: [ :each | textMorph addSegment: each]
]

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyMethodCodeEditorToolMorph >> supportsCriticDecoration [
	^true
]

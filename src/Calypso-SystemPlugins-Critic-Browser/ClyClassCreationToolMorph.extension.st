Extension { #name : 'ClyClassCreationToolMorph' }

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyClassCreationToolMorph >> criticAnalysisScope [

	^ ClyPackageScope of: self package in: self browser navigationEnvironment
]

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyClassCreationToolMorph >> decorateByCritic: aCriticTool [
]

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ClyClassCreationToolMorph >> supportsCriticDecoration [

	^ self package isNotNil
]

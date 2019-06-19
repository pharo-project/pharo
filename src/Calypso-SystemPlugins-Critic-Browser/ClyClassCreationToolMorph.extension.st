Extension { #name : #ClyClassCreationToolMorph }

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyClassCreationToolMorph >> criticAnalysisScope [

	^ClyPackageScope of: package in: browser navigationEnvironment
]

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyClassCreationToolMorph >> decorateByCritic: aCriticTool [
]

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyClassCreationToolMorph >> supportsCriticDecoration [
	^package notNil
]

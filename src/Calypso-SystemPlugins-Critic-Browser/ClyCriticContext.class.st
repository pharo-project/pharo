"
I am a context of critique decorators tool representing selected critiques

 
Internal Representation and Key Implementation Points.

    Instance Variables
	analyzedObject:		<Object>
"
Class {
	#name : 'ClyCriticContext',
	#superclass : 'ClyBrowserContext',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'selection strategy' }
ClyCriticContext class >> selectionStrategy [
	<classAnnotation>

	^ClyContextSelectionStrategyAnnotation for: ClyCriticDecorator
]

{ #category : 'accessing' }
ClyCriticContext >> analyzedObject [
	^ tool analyzedObject
]

{ #category : 'accessing' }
ClyCriticContext >> browser [
	^tool browser
]

{ #category : 'refactoring support' }
ClyCriticContext >> refactoringScopes [
	^self browser allNavigationScopes
]

{ #category : 'accessing' }
ClyCriticContext >> selectedCritique [
	^self lastSelectedItem actualObject
]

"
I am a context of critique decorators tool representing selected critiques

 
Internal Representation and Key Implementation Points.

    Instance Variables
	analyzedObject:		<Object>
"
Class {
	#name : #ClyCriticContext,
	#superclass : #ClyBrowserContext,
	#instVars : [
		'analyzedObject'
	],
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #'selection strategy' }
ClyCriticContext class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategy for: ClyCriticDecorator
]

{ #category : #accessing }
ClyCriticContext >> analyzedObject [
	^ tool analyzedObject
]

{ #category : #accessing }
ClyCriticContext >> browser [
	^tool browser
]

{ #category : #'command execution' }
ClyCriticContext >> executeCommand: aCommand by: aCommandActivator [

	aCommand isComplexRefactoring ifTrue: [
		aCommand confirmRefactoringInContext: self by: aCommandActivator].
	
	aCommand execute
]

{ #category : #'refactoring support' }
ClyCriticContext >> refactoringScopes [
	^self browser allNavigationScopes
]

{ #category : #accessing }
ClyCriticContext >> selectedCritique [
	^self lastSelectedItem actualObject
]

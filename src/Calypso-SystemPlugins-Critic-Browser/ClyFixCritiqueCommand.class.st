"
Automatically resolve the issue

I am a command to fix given critique by applying proposed changes.
I open changes preview on them
"
Class {
	#name : 'ClyFixCritiqueCommand',
	#superclass : 'ClyCritiqueCommand',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'testing' }
ClyFixCritiqueCommand >> canBeExecuted [

	super canBeExecuted ifFalse: [ ^ false ].

	^ self context lastSelectedItem isMarkedWith: ClyCritiqueFixableTag
]

{ #category : 'execution' }
ClyFixCritiqueCommand >> executeRefactoring [

	(StChangesBrowserPresenter changes: { self critique change }) open
]

{ #category : 'initialization' }
ClyFixCritiqueCommand >> iconName [
	^#repair
]

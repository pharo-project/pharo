"
I am a command to fix given critique by applying proposed changes.
I open changes preview on them
"
Class {
	#name : #ClyFixCritiqueCommand,
	#superclass : #ClyCritiqueCommand,
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #execution }
ClyFixCritiqueCommand class >> canBeExecutedInContext: aCriticContext [
	(super canBeExecutedInContext: aCriticContext) ifFalse: [ ^false ].
	
	^aCriticContext lastSelectedItem isMarkedWith: ClyCritiqueFixableTag
]

{ #category : #initialization }
ClyFixCritiqueCommand >> defaultMenuIconName [
	^#repairIcon
]

{ #category : #'table decoration' }
ClyFixCritiqueCommand >> description [
	^'Automatically resolve the issue'
]

{ #category : #execution }
ClyFixCritiqueCommand >> execute [

	(ChangesBrowser changes: { critique change }) open
]

"
I am a critique that will propose to the user to remove an instance variable.
"
Class {
	#name : 'ReRemoveInstanceVariableCritique',
	#superclass : 'ReAbstractCritique',
	#category : 'Renraku-Critiques',
	#package : 'Renraku',
	#tag : 'Critiques'
}

{ #category : 'instance creation' }
ReRemoveInstanceVariableCritique class >> for: anEntity instanceVariable: anInstanceVariable by: aRule [

	^ (self for: anEntity by: aRule)
		tinyHint: anInstanceVariable;
		yourself
]

{ #category : 'accessing' }
ReRemoveInstanceVariableCritique >> change [

	^ (RBRefactoryChangeFactory instance)
		removeInstanceVariable: self tinyHint from: self entity
]

{ #category : 'testing' }
ReRemoveInstanceVariableCritique >> providesChange [

	^ true
]

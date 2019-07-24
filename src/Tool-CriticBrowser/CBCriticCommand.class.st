Class {
	#name : #CBCriticCommand,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #testing }
CBCriticCommand class >> isAbstract [
	^ self = CBCriticCommand
]

{ #category : #hooks }
CBCriticCommand >> canBeRun [
	^ self selectedCritic isNotNil
]

{ #category : #hooks }
CBCriticCommand >> selectedCritic [
	^ self context selectedCritic
]

Class {
	#name : #CBRuleCommand,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #testing }
CBRuleCommand class >> isAbstract [
	^ self = CBRuleCommand
]

{ #category : #hooks }
CBRuleCommand >> canBeRun [
	^ self selectedRule isNotNil
]

{ #category : #hooks }
CBRuleCommand >> selectedRule [
	^ self context selectedRule
]

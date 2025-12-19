Class {
	#name : 'CoDependencyPackageScopedSelectorTestNodeStub',
	#superclass : 'Object',
	#instVars : [
		'message',
		'receiver',
		'method'
	],
	#category : 'HeuristicCompletion-Tests-Mocks',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Mocks'
}

{ #category : 'as yet unclassified' }
CoDependencyPackageScopedSelectorTestNodeStub class >> messageWithReceiver: aReceiver [

    ^ self new
          beMessageWithReceiver: aReceiver;
          yourself
]

{ #category : 'as yet unclassified' }
CoDependencyPackageScopedSelectorTestNodeStub class >> methodNode [

    ^ self new
          beMethod;
          yourself
]

{ #category : 'as yet unclassified' }
CoDependencyPackageScopedSelectorTestNodeStub >> beMessageWithReceiver: aReceiver [

    message := true.
    receiver := aReceiver
]

{ #category : 'as yet unclassified' }
CoDependencyPackageScopedSelectorTestNodeStub >> beMethod [

    method := true
]

{ #category : 'as yet unclassified' }
CoDependencyPackageScopedSelectorTestNodeStub >> isMessage [

    ^ message = true
]

{ #category : 'as yet unclassified' }
CoDependencyPackageScopedSelectorTestNodeStub >> isMethod [

    ^ method = true
]

{ #category : 'as yet unclassified' }
CoDependencyPackageScopedSelectorTestNodeStub >> receiver [

    ^ receiver ifNil: [ receiver := Object new ]
]

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

{ #category : 'instance creation' }
CoDependencyPackageScopedSelectorTestNodeStub class >> messageWithReceiver: aReceiver [

    ^ self new
          beMessageWithReceiver: aReceiver;
          yourself
]

{ #category : 'instance creation' }
CoDependencyPackageScopedSelectorTestNodeStub class >> methodNode [

    ^ self new
          beMethod;
          yourself
]

{ #category : 'configuring' }
CoDependencyPackageScopedSelectorTestNodeStub >> beMessageWithReceiver: aReceiver [

    message := true.
    receiver := aReceiver
]

{ #category : 'configuring' }
CoDependencyPackageScopedSelectorTestNodeStub >> beMethod [

    method := true
]

{ #category : 'testing' }
CoDependencyPackageScopedSelectorTestNodeStub >> isMessage [

    ^ message = true
]

{ #category : 'testing' }
CoDependencyPackageScopedSelectorTestNodeStub >> isMethod [

    ^ method = true
]

{ #category : 'accessing' }
CoDependencyPackageScopedSelectorTestNodeStub >> receiver [

    ^ receiver ifNil: [ receiver := Object new ]
]

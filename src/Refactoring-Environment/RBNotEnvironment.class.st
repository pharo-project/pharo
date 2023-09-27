"
I am the complement of RBEnvironments, a logical NOT. That is: 
entity A is in this environment if it is in NOT in the environment I am constructed from.

Do not construct instances of me directly, use method #not for an existing environment:
env1 not -> a RBNotEnvironment.
"
Class {
	#name : 'RBNotEnvironment',
	#superclass : 'RBBrowserEnvironmentWrapper',
	#category : 'Refactoring-Environment',
	#package : 'Refactoring-Environment'
}

{ #category : 'testing' }
RBNotEnvironment >> definesClass: aClass [
	^ (environment definesClass: aClass) not
]

{ #category : 'description' }
RBNotEnvironment >> description [
	^ '(', environment shortDescription, ') not'
]

{ #category : 'testing' }
RBNotEnvironment >> includesClass: aClass [
	(environment includesClass: aClass) ifFalse: [^true].
	^false
]

{ #category : 'testing' }
RBNotEnvironment >> includesProtocol: aProtocol in: aClass [

	(aClass hasProtocol: aProtocol) ifFalse: [ ^ false ].

	^ (aClass selectorsInProtocol: aProtocol) anySatisfy: [ :selector | self includesSelector: selector in: aClass ]
]

{ #category : 'testing' }
RBNotEnvironment >> includesSelector: aSelector in: aClass [
	^(environment includesSelector: aSelector in: aClass) not
]

{ #category : 'environments' }
RBNotEnvironment >> not [
	^environment
]

{ #category : 'printing' }
RBNotEnvironment >> storeOn: aStream [
	environment storeOn: aStream.
	aStream nextPutAll: ' not'
]

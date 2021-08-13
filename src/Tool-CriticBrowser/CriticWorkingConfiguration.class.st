"
A CriticWorkspace holds the current worspace for the CriticBrowser.

It's composed of the set of rules to check (as a composite rule) and the environment to work in.
"
Class {
	#name : #CriticWorkingConfiguration,
	#superclass : #Object,
	#instVars : [
		'rule',
		'environment',
		'removeTestCase',
		'logInManifest'
	],
	#classVars : [
		'Current'
	],
	#category : #'Tool-CriticBrowser-Utilities'
}

{ #category : #'instance creation' }
CriticWorkingConfiguration class >> current [
	"please use CBCritiqueWorkingConfiguration>>current instead"	
	self deprecated: 'use CBCritiqueWorkingConfiguration>>current'.
	
	^ Current ifNil: [ Current  := self basicNew initialize ].
]

{ #category : #testing }
CriticWorkingConfiguration class >> exists [
	"please use CBCritiqueWorkingConfiguration>>exists instead"	
	self deprecated: 'use CBCritiqueWorkingConfiguration>>exists'.
	^ Current notNil
]

{ #category : #'instance creation' }
CriticWorkingConfiguration class >> isDeprecated [ 
	"We do not need this class anymore. Use CBCritiqueWorkingConfiguration instead" 

	^true
]

{ #category : #'instance creation' }
CriticWorkingConfiguration class >> new [
	"please use CBCritiqueWorkingConfiguration>>new instead"	
	self deprecated: 'use CBCritiqueWorkingConfiguration>>new'. 
	^ self shouldNotImplement
]

{ #category : #initialization }
CriticWorkingConfiguration class >> reset [
	"please use CBCritiqueWorkingConfiguration>>exists reset"	
	self deprecated: 'use CBCritiqueWorkingConfiguration>>reset'.
	Current := nil
]

{ #category : #accessing }
CriticWorkingConfiguration >> environment [
	^ environment
]

{ #category : #accessing }
CriticWorkingConfiguration >> environment: anObject [
	environment := anObject
]

{ #category : #accessing }
CriticWorkingConfiguration >> logInManifest [
	^ logInManifest
]

{ #category : #accessing }
CriticWorkingConfiguration >> logInManifest: anObject [
	logInManifest := anObject
]

{ #category : #accessing }
CriticWorkingConfiguration >> removeTestCase [
	^ removeTestCase
]

{ #category : #accessing }
CriticWorkingConfiguration >> removeTestCase: aBoolean [
	removeTestCase := aBoolean
]

{ #category : #accessing }
CriticWorkingConfiguration >> rule [
	^ rule
]

{ #category : #accessing }
CriticWorkingConfiguration >> rule: anObject [
	rule := anObject
]

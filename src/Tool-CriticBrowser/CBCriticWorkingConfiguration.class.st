"
A CriticWorkspace holds the current worspace for the CriticBrowser.

It's composed of the set of rules to check (as a composite rule) and the environment to work in.
"
Class {
	#name : #CBCriticWorkingConfiguration,
	#superclass : #Object,
	#instVars : [
		'environment',
		'removeTestCase',
		'logInManifest',
		'rules'
	],
	#classVars : [
		'Current'
	],
	#category : #'Tool-CriticBrowser-Utilities'
}

{ #category : #'instance creation' }
CBCriticWorkingConfiguration class >> current [

	^ Current ifNil: [ Current  := self basicNew initialize ].
]

{ #category : #testing }
CBCriticWorkingConfiguration class >> exists [

	^ Current notNil
]

{ #category : #'instance creation' }
CBCriticWorkingConfiguration class >> new [

	^ self shouldNotImplement
]

{ #category : #initialization }
CriticWorkingConfiguration class >> reset [

	Current := nil
]

{ #category : #accessing }
CBCriticWorkingConfiguration >> environment [
	^ environment
]

{ #category : #accessing }
CBCriticWorkingConfiguration >> environment: anObject [
	environment := anObject
]

{ #category : #accessing }
CBCriticWorkingConfiguration >> logInManifest [
	^ logInManifest
]

{ #category : #accessing }
CBCriticWorkingConfiguration >> logInManifest: anObject [
	logInManifest := anObject
]

{ #category : #accessing }
CBCriticWorkingConfiguration >> removeTestCase [
	^ removeTestCase
]

{ #category : #accessing }
CBCriticWorkingConfiguration >> removeTestCase: aBoolean [
	removeTestCase := aBoolean
]

{ #category : #accessing }
CBCriticWorkingConfiguration >> rules [
	^ rules
]

{ #category : #accessing }
CBCriticWorkingConfiguration >> rules: anObject [
	rules := anObject
]

"
I'm a class to add support for iceberg to metacello.
"
Class {
	#name : #IceMetacelloPharoPlatform,
	#superclass : #MetacelloPharoPlatform,
	#category : #'Iceberg-Metacello-Integration'
}

{ #category : #initialization }
IceMetacelloPharoPlatform class >> initialize [
	 self select
]

{ #category : #testing }
IceMetacelloPharoPlatform >> canHandleType: type [
	^ Iceberg enableMetacelloIntegration 
		and: [ IceMetacelloRepositoryType canHandleType: type ]
]

{ #category : #'repository creation' }
IceMetacelloPharoPlatform >> createRepository: aRepositorySpec [
	(self canHandleType: aRepositorySpec type)
		ifTrue: [ ^ Iceberg mcRepositoryFor: aRepositorySpec description ].

	^ super createRepository: aRepositorySpec
]

{ #category : #'repository creation' }
IceMetacelloPharoPlatform >> extractTypeFromDescription: description [

	(description beginsWith: 'ftp://') ifTrue: [ ^'ftp' ].
	IceMetacelloRepositoryType allTypes 
		detect:  [ :each | each isSuitableForLocation: description  ] 
		ifFound: [ :class | ^ class type ].
	^ super extractTypeFromDescription: description
]

{ #category : #enumerating }
IceMetacelloPharoPlatform >> withMetacelloLoadSessionDo: aBlock [ 
	| context |

	"Use current context or create a new one"
	context := IceMetacelloLoadSessionContext value 
		ifNil: [ IceMetacelloLoadSession new ].

	"pass it"
	IceMetacelloLoadSessionContext
		value: context 
		during: aBlock
]

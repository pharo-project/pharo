"
I am some kind of Utility to centralize some behavior such as the display of progress bar but also a hook to let some other projects such as Iceberg extend Metacello behaviors.
"
Class {
	#name : 'MetacelloPlatform',
	#superclass : 'Object',
	#classVars : [
		'Current'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'accessing' }
MetacelloPlatform class >> current [
	Current
		ifNil: [Current := MetacelloPlatform new].
	^ Current
]

{ #category : 'class initialization' }
MetacelloPlatform class >> initialize [

	self select
]

{ #category : 'class initialization' }
MetacelloPlatform class >> select [
  Current := self new
]

{ #category : 'class initialization' }
MetacelloPlatform class >> unselect [
  MetacelloPlatform current class = self
    ifTrue: [ Current := nil ]
]

{ #category : 'repository creation' }
MetacelloPlatform >> createRepository: aRepositorySpec [

	^ MCRepository newRepositoryFromSpec: aRepositorySpec
]

{ #category : 'repository creation' }
MetacelloPlatform >> extractTypeFromDescription: description [

	description ifNil: [ ^ nil ].
	((description beginsWith: '/') or: [ description second = $: ]) ifTrue: [ ^ 'directory' ].
	(description beginsWith: 'dictionary://') ifTrue: [ ^ 'dictionary' ].
	(description beginsWith: 'filetree://') ifTrue: [ ^ 'filetree' ].
	(description beginsWith: 'tonel://') ifTrue: [ ^ 'tonel' ].
	(description beginsWith: 'github://') ifTrue: [ ^ 'github' ].
	(description beginsWith: 'gitorious://') ifTrue: [ ^ 'gitorious' ].
	(description beginsWith: 'bitbucket://') ifTrue: [ ^ 'bitbucket' ].
	^ 'http'
]

{ #category : 'execution' }
MetacelloPlatform >> withMetacelloLoadSessionDo: aBlock [ 

	aBlock value
]

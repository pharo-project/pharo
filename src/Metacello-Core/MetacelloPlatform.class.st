"
I am some kind of Utility to centralize some behavior such as the display of progress bar but also a hook to let some other projects such as Iceberg extend Metacello behaviors.
"
Class {
	#name : 'MetacelloPlatform',
	#superclass : 'Object',
	#instVars : [
		'bypassProgressBars'
	],
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

{ #category : 'accessing' }
MetacelloPlatform >> bypassProgressBars [

	^ bypassProgressBars ifNil: [ bypassProgressBars := false ]
]

{ #category : 'accessing' }
MetacelloPlatform >> bypassProgressBars: anObject [
	bypassProgressBars := anObject
]

{ #category : 'repository creation' }
MetacelloPlatform >> createRepository: aRepositorySpec [

	^ MCRepository newRepositoryFromSpec: aRepositorySpec
]

{ #category : 'notification' }
MetacelloPlatform >> do: aBlock displaying: aString [

	self bypassProgressBars ifTrue: [ ^ aBlock value ].

	aString displayProgressFrom: 0 to: 2 during: [ :bar |
		bar value: 1.
		aBlock value.
		bar value: 2 ]
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

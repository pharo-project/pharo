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

{ #category : 'initialize-release' }
MetacelloPlatform class >> select [
  Current := self new
]

{ #category : 'initialize-release' }
MetacelloPlatform class >> unselect [
  MetacelloPlatform current class = self
    ifTrue: [ Current := nil ]
]

{ #category : 'accessing' }
MetacelloPlatform >> bypassProgressBars [

	bypassProgressBars == nil ifTrue: [ bypassProgressBars := false ].
	^ bypassProgressBars
]

{ #category : 'accessing' }
MetacelloPlatform >> bypassProgressBars: anObject [
	bypassProgressBars := anObject
]

{ #category : 'notification' }
MetacelloPlatform >> collection: aCollection do: aBlock displaying: aString [

	self bypassProgressBars ifTrue: [ ^ aCollection do: aBlock ].
	aCollection do: aBlock displayingProgress: aString
]

{ #category : 'reflection' }
MetacelloPlatform >> copyClass: oldClass as: newName inCategory: newCategoryName [

	| copysName class newDefinition |
	copysName := newName asSymbol.
	copysName = oldClass name ifTrue: [ ^ oldClass ].
	(Smalltalk globals includesKey: copysName) ifTrue: [ ^ self error: copysName , ' already exists' ].
	newDefinition := oldClass definition copyReplaceAll: '#' , oldClass name asString with: '#' , copysName asString printString.
	newDefinition := newDefinition copyReplaceAll: 'category: ' , oldClass category asString printString with: 'category: ' , newCategoryName printString.
	class := self class compiler
		         logged: true;
		         evaluate: newDefinition.
	class class instanceVariableNames: oldClass class instanceVariablesString.
	class copyAllMethodsFrom: oldClass.
	class class copyAllMethodsFrom: oldClass class.
	class category: newCategoryName.
	^ class
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

{ #category : 'caching' }
MetacelloPlatform >> primeStackCacheWith: aDictionary doing: noArgBlock [

	self 
		useStackCacheDuring: [:dict | ^noArgBlock value ] 
		defaultDictionary: aDictionary
]

{ #category : 'caching' }
MetacelloPlatform >> stackCacheFor: cacheName at: key doing: aBlock [

	^self stackCacheFor: cacheName cacheClass: Dictionary at: key doing: aBlock
]

{ #category : 'caching' }
MetacelloPlatform >> stackCacheFor: cacheName cacheClass: cacheClass at: key doing: aBlock [

	self 
		useStackCacheDuring: [:dict | | cache |
			cache := dict at: cacheName ifAbsent: [].
			cache ~~ nil
				ifTrue: [ | value hasEntry |
					hasEntry := true.
					value := cache at: key ifAbsent: [ hasEntry := false ].
					hasEntry ifTrue: [ ^value ]]
				ifFalse: [ 
					cache := cacheClass new.
					dict at: cacheName put: cache ].
			^aBlock value: cache ] 
		defaultDictionary: nil
]

{ #category : 'caching' }
MetacelloPlatform >> useStackCacheDuring: aBlock defaultDictionary: defaultDictionary [

	| dict |
	dict := MetacelloStackCacheNotification signal ifNil: [ defaultDictionary ifNil: [ Dictionary new ] ].

	[
	[ ^ aBlock value: dict ]
		on: MetacelloStackCacheNotification
		do: [ :ex | ex resume: dict ] ]
		on: MetacelloClearStackCacheNotification
		do: [ :ex |
			| keys |
			keys := ex cacheNames ifNil: [ dict keys ].
			keys do: [ :k |
				(dict includesKey: k) ifTrue: [
					| c |
					c := dict at: k.
					c keys do: [ :ck | c removeKey: ck ].
					dict removeKey: k ] ].
			ex resume ]
]

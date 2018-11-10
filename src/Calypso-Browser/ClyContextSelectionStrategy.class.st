"
I describe the way the context of browser selection should be active


"
Class {
	#name : #ClyContextSelectionStrategy,
	#superclass : #ClassAnnotation,
	#instVars : [
		'targetItemType'
	],
	#category : #'Calypso-Browser-Contexts'
}

{ #category : #'instance creation' }
ClyContextSelectionStrategy class >> for: aBrowserClass selectionType: aScopeClass [
	^(self for: aBrowserClass)
		targetItemType: aScopeClass
]

{ #category : #query }
ClyContextSelectionStrategy class >> selectContextOf: aSelection for: aBrowser [

	self activeInstancesInContext: aBrowser do: [ :strategy | 
		(strategy shouldBeAppliedFor: aSelection) ifTrue: [ 
			^strategy createBrowserContextOf: aSelection for: aBrowser]].
	
	^ClyUnknownBrowserContext for: aBrowser
]

{ #category : #query }
ClyContextSelectionStrategy class >> selectContextOfSingleItem: aDataSourceItem for: aBrowser [

	self activeInstancesInContext: aBrowser do: [ :strategy | 
		(strategy shouldBeAppliedFor: aDataSourceItem) ifTrue: [ 
			^strategy createBrowserContextOfSingleItem: aDataSourceItem for: aBrowser]].
	
	^ClyUnknownBrowserContext for: aBrowser
]

{ #category : #'context creation' }
ClyContextSelectionStrategy >> createBrowserContextOf: aSelection for: aBrowser [

	^annotatedClass ofSelection: aSelection for: aBrowser
]

{ #category : #'context creation' }
ClyContextSelectionStrategy >> createBrowserContextOfSingleItem: aDataSourceItem for: aBrowser [

	^annotatedClass of: {aDataSourceItem} for: aBrowser
]

{ #category : #printing }
ClyContextSelectionStrategy >> printOn: aStream [
	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: annotatedClass name.
	targetItemType ifNotNil: [ 
		aStream
			nextPutAll: '; ';
			nextPutAll: targetItemType name].
	aStream
		nextPut: $)
]

{ #category : #testing }
ClyContextSelectionStrategy >> shouldBeAppliedFor: aSelectionOrItem [

	targetItemType ifNil: [ 
	"if no scope specified strategy will be applied to any selection independently from it kind"	
		^true ].
	
	^aSelectionOrItem isBasedOnItemType: targetItemType
]

{ #category : #accessing }
ClyContextSelectionStrategy >> targetItemType [
	^ targetItemType
]

{ #category : #accessing }
ClyContextSelectionStrategy >> targetItemType: aClass [
	targetItemType := aClass
]

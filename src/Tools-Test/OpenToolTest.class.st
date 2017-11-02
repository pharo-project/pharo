"
SUnit tests for opening of tools
"
Class {
	#name : #OpenToolTest,
	#superclass : #UITestCase,
	#category : 'Tools-Test-Base'
}

{ #category : #helpers }
OpenToolTest >> openInspectorOn: anObject [

	^ Smalltalk tools inspector inspector: anObject
]

{ #category : #'test inspect' }
OpenToolTest >> testInspectArray [
	| inspector |
	inspector := self openInspectorOn: (1 to: 1000) asArray.
	inspector changed.
	inspector close.
]

{ #category : #'test inspect' }
OpenToolTest >> testInspectClass [
	| inspector |
	inspector :=  self openInspectorOn: Object.
	inspector changed.
	inspector close.
]

{ #category : #'test inspect' }
OpenToolTest >> testInspectDictionary [
	| inspector |
	inspector :=  self openInspectorOn: Dictionary new.
	inspector changed.
	inspector close.
]

{ #category : #'test inspect' }
OpenToolTest >> testInspectInteger [
	| inspector |
	inspector :=  self openInspectorOn: 1.
	inspector changed.
	inspector close.
]

{ #category : #'test inspect' }
OpenToolTest >> testInspectTraitClass [
	| inspector |
	inspector :=  self openInspectorOn: TBehavior.
	inspector changed.
	inspector close.
]

{ #category : #'test browse' }
OpenToolTest >> testOpenBrowseOnClass [
	| browser |
	browser := Object browse.
	browser changed.
	browser close.
]

{ #category : #'test browse' }
OpenToolTest >> testOpenBrowseOnInstalledTraitMethod [
	| browser |
	browser := (Class>>#hasClassSide) browse.
	browser changed.
	browser close.
]

{ #category : #'test browse' }
OpenToolTest >> testOpenBrowseOnMethod [
	| browser |
	browser := (Object>>#yourself) browse.
	browser changed.
	browser close.
]

{ #category : #'test browse' }
OpenToolTest >> testOpenBrowseOnTraitMethod [
	| browser |
	browser := (TBehavior>>#name) browse.
	browser changed.
	browser close.
]

{ #category : #'test senders' }
OpenToolTest >> testSendersOfAt [
	| senders |
	senders := SystemNavigation default browseAllSendersOf: #name.
	senders changed.
	senders close.
]

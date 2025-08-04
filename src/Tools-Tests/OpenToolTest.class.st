"
SUnit tests for opening of tools
"
Class {
	#name : 'OpenToolTest',
	#superclass : 'AbstractUITestCase',
	#category : 'Tools-Tests-Base',
	#package : 'Tools-Tests',
	#tag : 'Base'
}

{ #category : 'utilities' }
OpenToolTest >> openInspectorOn: anObject [

	^ (Smalltalk tools toolNamed: #inspector) inspect: anObject
]

{ #category : 'tests - implementors' }
OpenToolTest >> testImplementorsOfAt [
	| implementors |
	implementors := SystemNavigation default browseAllImplementorsOf: #name.
	implementors changed.
	implementors close
]

{ #category : 'tests - inspect' }
OpenToolTest >> testInspectArray [
	| inspector |
	inspector := self openInspectorOn: (1 to: 1000) asArray.
	inspector changed.
	inspector close
]

{ #category : 'tests - inspect' }
OpenToolTest >> testInspectClass [
	| inspector |
	inspector := self openInspectorOn: Object.
	inspector changed.
	inspector close
]

{ #category : 'tests - inspect' }
OpenToolTest >> testInspectDictionary [
	| inspector |
	inspector := self openInspectorOn: Dictionary new.
	inspector changed.
	inspector close
]

{ #category : 'tests - inspect' }
OpenToolTest >> testInspectInteger [
	| inspector |
	inspector := self openInspectorOn: 1.
	inspector changed.
	inspector close
]

{ #category : 'tests - inspect' }
OpenToolTest >> testInspectTraitClass [
	| inspector |
	inspector := self openInspectorOn: TSortable.
	inspector changed.
	inspector close
]

{ #category : 'tests - browse' }
OpenToolTest >> testOpenBrowseOnClass [
	| browser |
	browser := Object browse.
	browser changed.
	browser close
]

{ #category : 'tests - browse' }
OpenToolTest >> testOpenBrowseOnInstalledTraitMethod [
	| browser |
	browser := (Class>>#hasClassSide) browse.
	browser changed.
	browser close
]

{ #category : 'tests - browse' }
OpenToolTest >> testOpenBrowseOnMethod [
	| browser |
	browser := (Object>>#yourself) browse.
	browser changed.
	browser close
]

{ #category : 'tests - browse' }
OpenToolTest >> testOpenBrowseOnTraitMethod [
	| browser |
	browser := (TSortable>>#isSorted) browse.
	browser changed.
	browser close
]

{ #category : 'tests - senders' }
OpenToolTest >> testSendersOfAt [
	| senders |
	senders := SystemNavigation default browseAllSendersOf: #name.
	senders changed.
	senders close
]

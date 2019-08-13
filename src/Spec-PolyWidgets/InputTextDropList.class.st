"
I am an elementary Spec widget that associates an TextInputField and a DropListPresenter.

self example

I provide accessors dropList and input and shortcuts for the most common methods of their api.
"
Class {
	#name : #InputTextDropList,
	#superclass : #ComposablePresenter,
	#instVars : [
		'dropList',
		'input'
	],
	#category : #'Spec-PolyWidgets-Elementary'
}

{ #category : #specs }
InputTextDropList class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #input origin: 0 @ 0 corner: 0.3 @ 1;
				add: #dropList origin: 0.3 @ 0 corner: 1 @ 1 ];
			yourself
]

{ #category : #example }
InputTextDropList class >> example [
	<sampleInstance>
	| ex |
	ex := self new.
	ex
		ghostText: 'a Number';
		displayBlock: [ :item | item asString ];
		items: {'Potatoes' . 'Carrots' . 'Onions'};
		extent: 350 @ 50;
		title: 'InputTextDropList example';
		openWithSpec.
	^ ex
]

{ #category : #'api-shortcuts' }
InputTextDropList >> acceptOnCR: aBoolean [
	self input acceptOnCR: aBoolean
]

{ #category : #'api-shortcuts' }
InputTextDropList >> beEncrypted [
	self input beEncrypted
]

{ #category : #'api-shortcuts' }
InputTextDropList >> displayBlock: aBlock [
	self dropList displayBlock: aBlock
]

{ #category : #accessing }
InputTextDropList >> dropList [
	^ dropList
]

{ #category : #'api-shortcuts' }
InputTextDropList >> emptyList [
	self dropList emptyList
]

{ #category : #'api-shortcuts' }
InputTextDropList >> encrypted: aBoolean [
	self input encrypted: aBoolean
]

{ #category : #'api-shortcuts' }
InputTextDropList >> entryCompletion: anEntryCompletion [
	self input entryCompletion: anEntryCompletion
]

{ #category : #'api-shortcuts' }
InputTextDropList >> ghostText: aText [
	self input ghostText: aText
]

{ #category : #initialization }
InputTextDropList >> initializeWidgets [
	input := self instantiate: TextInputFieldPresenter.
	dropList := self instantiate: DropListPresenter.
	self focusOrder
		add: input;
		add: dropList
]

{ #category : #accessing }
InputTextDropList >> input [
	^ input
]

{ #category : #'api-shortcuts' }
InputTextDropList >> items: aList [
	self dropList items: aList
]

{ #category : #'api-shortcuts' }
InputTextDropList >> listSize [
	self dropList listSize
]

{ #category : #'api-shortcuts' }
InputTextDropList >> resetSelection [
	self dropList resetSelection
]

{ #category : #'api-shortcuts' }
InputTextDropList >> selectedItem [
	self dropList selectedItem
]

{ #category : #'api-shortcuts' }
InputTextDropList >> setIndex: anIndex [
	self dropList setIndex: anIndex
]

{ #category : #'api-shortcuts' }
InputTextDropList >> text [
	self input text
]

{ #category : #'api-shortcuts' }
InputTextDropList >> text: aText [
	self input text: aText
]

{ #category : #'events-shortcuts' }
InputTextDropList >> whenSelectedItemChanged: aBlock [
	self dropList whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
InputTextDropList >> whenSelectionChanged: aBlock [
	self dropList whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
InputTextDropList >> whenSelectionIndexChanged: aBlock [
	self dropList whenSelectionIndexChanged: aBlock
]

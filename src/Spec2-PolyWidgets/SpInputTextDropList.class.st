"
I am an elementary Spec widget that associates an TextInputField and a DropListPresenter.

self example

I provide accessors dropList and input and shortcuts for the most common methods of their api.
"
Class {
	#name : #SpInputTextDropList,
	#superclass : #SpPresenter,
	#instVars : [
		'dropList',
		'input'
	],
	#category : #'Spec2-PolyWidgets-Elementary'
}

{ #category : #specs }
SpInputTextDropList class >> defaultSpec [
	<spec: #default>
	^ SpLayout composed
		newRow: [ :row | 
			row
				add: #input origin: 0 @ 0 corner: 0.3 @ 1;
				add: #dropList origin: 0.3 @ 0 corner: 1 @ 1 ];
			yourself
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> acceptOnCR: aBoolean [
	self input acceptOnCR: aBoolean
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> beEncrypted [
	self input beEncrypted
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> displayBlock: aBlock [
	self dropList displayBlock: aBlock
]

{ #category : #accessing }
SpInputTextDropList >> dropList [
	^ dropList
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> emptyList [
	self dropList emptyList
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> encrypted: aBoolean [
	self input encrypted: aBoolean
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> entryCompletion: anEntryCompletion [
	self input entryCompletion: anEntryCompletion
]

{ #category : #initialization }
SpInputTextDropList >> initializeWidgets [
	input := self instantiate: SpTextInputFieldPresenter.
	dropList := self instantiate: SpDropListPresenter.
	self focusOrder
		add: input;
		add: dropList
]

{ #category : #accessing }
SpInputTextDropList >> input [
	^ input
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> items: aList [
	self dropList items: aList
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> listSize [
	self dropList listSize
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> placeholder: aText [
	self input placeholder: aText
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> resetSelection [
	self dropList resetSelection
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> selectedItem [
	self dropList selectedItem
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> setIndex: anIndex [
	self dropList setIndex: anIndex
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> text [
	self input text
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> text: aText [
	self input text: aText
]

{ #category : #'events-shortcuts' }
SpInputTextDropList >> whenSelectedItemChanged: aBlock [
	self dropList whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpInputTextDropList >> whenSelectionChanged: aBlock [
	self dropList whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpInputTextDropList >> whenSelectionIndexChanged: aBlock [
	self dropList whenSelectionIndexChanged: aBlock
]

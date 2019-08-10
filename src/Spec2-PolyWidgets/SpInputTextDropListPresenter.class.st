"
I am an elementary Spec widget that associates an TextInputField and a DropListPresenter.

self example

I provide accessors dropList and input and shortcuts for the most common methods of their api.
"
Class {
	#name : #SpInputTextDropListPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'dropList',
		'input'
	],
	#category : #'Spec2-PolyWidgets-Elementary'
}

{ #category : #specs }
SpInputTextDropListPresenter class >> defaultSpec [
	<spec: #default>
	^ SpLayout composed
		newRow: [ :row | 
			row
				add: #input origin: 0 @ 0 corner: 0.3 @ 1;
				add: #dropList origin: 0.3 @ 0 corner: 1 @ 1 ];
			yourself
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> acceptOnCR: aBoolean [
	self input acceptOnCR: aBoolean
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> beEncrypted [
	self input beEncrypted
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> displayBlock: aBlock [
	self dropList displayBlock: aBlock
]

{ #category : #accessing }
SpInputTextDropListPresenter >> dropList [
	^ dropList
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> emptyList [
	self dropList emptyList
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> encrypted: aBoolean [
	self input encrypted: aBoolean
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> entryCompletion: anEntryCompletion [
	self input entryCompletion: anEntryCompletion
]

{ #category : #initialization }
SpInputTextDropListPresenter >> initializeWidgets [
	input := self instantiate: SpTextInputFieldPresenter.
	dropList := self instantiate: SpDropListPresenter.
	self focusOrder
		add: input;
		add: dropList
]

{ #category : #accessing }
SpInputTextDropListPresenter >> input [
	^ input
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> items: aList [
	self dropList items: aList
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> listSize [
	self dropList listSize
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> placeholder: aText [
	self input placeholder: aText
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> resetSelection [
	self dropList resetSelection
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> selectedItem [
	self dropList selectedItem
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> setIndex: anIndex [
	self dropList setIndex: anIndex
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> text [
	self input text
]

{ #category : #'api-shortcuts' }
SpInputTextDropListPresenter >> text: aText [
	self input text: aText
]

{ #category : #'events-shortcuts' }
SpInputTextDropListPresenter >> whenSelectedItemChanged: aBlock [
	self dropList whenSelectedItemChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpInputTextDropListPresenter >> whenSelectionChanged: aBlock [
	self dropList whenSelectionChanged: aBlock
]

{ #category : #'events-shortcuts' }
SpInputTextDropListPresenter >> whenSelectionIndexChanged: aBlock [
	self dropList whenSelectionIndexChanged: aBlock
]

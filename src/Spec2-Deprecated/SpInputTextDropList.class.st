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
	#category : #'Spec2-Deprecated-Obsolete'
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

{ #category : #examples }
SpInputTextDropList class >> example [
	<sampleInstance>
	| example |
	example := self new
		placeholder: 'a Number';
		display: [ :item | item asString ];
		items: {'Potatoes' . 'Carrots' . 'Onions'};
		extent: 350 @ 50;
		openWithSpec;
		yourself.
	example withWindowDo: [ :window | window title: 'InputTextDropList example' ].
	^ example
]

{ #category : #testing }
SpInputTextDropList class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
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
SpInputTextDropList >> display: aBlock [

	self dropList display: aBlock
]

{ #category : #'api-shortcuts' }
SpInputTextDropList >> displayBlock: aBlock [

	self
		deprecated: 'Please use #display: instead'
		transformWith: '`@receiver displayBlock: `@arg' 
						-> '`@receiver display: `@arg'.

	self dropList display: aBlock
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

{ #category : #'as yet unclassified' }
SpInputTextDropList >> ghostText: aText [
	self deprecated: 'This API is too tight to Morph. Use #placeholder: instead.' transformWith: '`@receiver ghostText: `@statement' -> '`@receiver placeholder: `@statement'.
	self placeholder: aText
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

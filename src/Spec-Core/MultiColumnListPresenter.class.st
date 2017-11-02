"
A MultiColumnListPresenter is a spec presenter for Multi columns list
"
Class {
	#name : #MultiColumnListPresenter,
	#superclass : #ListPresenter,
	#category : 'Spec-Core-Widgets'
}

{ #category : #specs }
MultiColumnListPresenter class >> adapterName [

	^ #MultiColumnListAdapter
]

{ #category : #specs }
MultiColumnListPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(MultiColumnListAdapter
		adapt: #(model))
]

{ #category : #morphic }
MultiColumnListPresenter >> listElementAt: anIndex [

	^ self wrapItem: (self listItems at: anIndex) index: anIndex 
]

{ #category : #morphic }
MultiColumnListPresenter >> setIndex: anIndex [

	self allowToSelect ifFalse: [ ^ self ].
	self okToChange ifFalse: [ ^ self ].
	selectionHolder index value: anIndex.
	selectionHolder selection value: (self listItems at: anIndex ifAbsent: [ nil ]).
]

{ #category : #api }
MultiColumnListPresenter >> setSelectedIndex: anIndex [
	"<api: #integer min: 0 max: #listSize getter: #selectedIndex registration: #whenSelectionIndexChanged:>"
	"Set the index of the item you want to be selected"
	| idx selection |
	
	self allowToSelect ifFalse: [ ^ self ].
	self okToChange ifFalse: [ ^ self ].
	self listSize isZero ifTrue:  [ ^self ].
	idx := anIndex min: self listSize.
	selection := self listItems 
					at: idx 
					ifAbsent: [ idx := 0.
								nil ].
	
	selectionHolder index value: idx.
	selectionHolder selection value: selection.
]

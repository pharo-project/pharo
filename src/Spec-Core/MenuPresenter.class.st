"
I am a simple presenter describing a menu.
See AbstractWidgetPresenter

I only contains a list of menu groups (MenuGroupPresenter). Each group is separated by a splitter.

I provide the following variables and their accessors
- addGroup: use the block in parameter to initialize a new group, then add it in my collection.
- addMenuGroup: add the group in parameter in my collection.
- menuGroups getter of my collection.


todo
- addAllFromPragma:target:
- applyTo: activate the shortcut of the items of my groups.
- autoRefresh
- buildWithSpecAsPopup
- fromSpec:
- icon
- neglect:
- openWithSpecAt:
- printOn:
- title , addTitle:

"
Class {
	#name : #MenuPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'groups',
		'title',
		'icon',
		'autoRefresh'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
MenuPresenter class >> adapterName [

	^ #MenuAdapter
]

{ #category : #specs }
MenuPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(MenuAdapter
		adapt: #(model))
]

{ #category : #specs }
MenuPresenter class >> popup [
	<spec>
	
	^ #(MenuAdapter
		adaptAsPopup: #(model))
]

{ #category : #'api-building' }
MenuPresenter >> addAllFromPragma: pragma target: target [

	self fromSpec: (PragmaMenuBuilder 
		pragmaKeyword: pragma
		model: target) menuSpec
]

{ #category : #api }
MenuPresenter >> addGroup: aBlock [
	| group |
	
	group := MenuGroupPresenter new.
	aBlock value: group.
	self addMenuGroup: group
]

{ #category : #private }
MenuPresenter >> addMenuGroup: aMenuGroup [

	groups add: aMenuGroup.
	self autoRefresh 
		ifNotNil: [ aMenuGroup autoRefresh: self autoRefresh ]
]

{ #category : #adding }
MenuPresenter >> addTitle: aString [
	title value: aString
]

{ #category : #api }
MenuPresenter >> applyTo: aModel [

	aModel applyMenuModel: self
]

{ #category : #api }
MenuPresenter >> autoRefresh [

	^ autoRefresh value
]

{ #category : #api }
MenuPresenter >> autoRefresh: aBoolean [

	autoRefresh value: aBoolean
]

{ #category : #api }
MenuPresenter >> buildWithSpecAsPopup [

	^ self buildWithSpec: #popup
]

{ #category : #'api-building' }
MenuPresenter >> fromSpec: aSpec [

	aSpec addToMenuModel: self
]

{ #category : #api }
MenuPresenter >> icon [

	^ icon value
]

{ #category : #api }
MenuPresenter >> icon: anIcon [

	icon value: anIcon
]

{ #category : #api }
MenuPresenter >> iconName: aSymbol [

	icon value: (self iconNamed: aSymbol)
]

{ #category : #initialization }
MenuPresenter >> initialize [

	super initialize.
	groups := OrderedCollection new asValueHolder.
	title := nil asValueHolder.
	icon := nil asValueHolder.
	autoRefresh := nil asValueHolder.
	autoRefresh whenChangedDo: [ :aBoolean | self menuGroups do: [ :each | each autoRefresh: aBoolean ] ]
]

{ #category : #api }
MenuPresenter >> menuGroups [

	^ groups value
]

{ #category : #api }
MenuPresenter >> neglect: aModel [

	aModel neglectMenuModel: self
]

{ #category : #api }
MenuPresenter >> openWithSpecAt: aPosition [

	self buildWithSpecAsPopup.
	self changed: #openAt: with: { aPosition }
]

{ #category : #printing }
MenuPresenter >> printOn: aStream [
	super printOn: aStream.
	self title value
		ifNotNil: [ :t | 
			aStream
				nextPutAll: ' ''';
				nextPutAll: t;
				nextPutAll: '''' ]
]

{ #category : #api }
MenuPresenter >> title [

	^ title value
]

{ #category : #api }
MenuPresenter >> title: aString [

	title value: aString
]

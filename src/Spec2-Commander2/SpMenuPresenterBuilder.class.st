"
I am a visitor that builds a Spec MenuPresenter from a commands and commands group.
"
Class {
	#name : #SpMenuPresenterBuilder,
	#superclass : #CmVisitor,
	#instVars : [
		'menuPresenter',
		'stack'
	],
	#category : #'Spec2-Commander2-Visitors'
}

{ #category : #accessing }
SpMenuPresenterBuilder class >> menuPresenterClass [
	^ SpMenuPresenter
]

{ #category : #private }
SpMenuPresenterBuilder >> fillItem: aMenuItem with: aCommand [

	aMenuItem
		name: aCommand name;
		description: aCommand description;
		icon: aCommand icon;
		action: [ aCommand execute ].

	aCommand hasShortcutKey ifTrue: [ 
		aMenuItem shortcut: aCommand shortcutKey ].
		
	^ aMenuItem
]

{ #category : #initialization }
SpMenuPresenterBuilder >> initialize [
	
	super initialize.
	self menuPresenter: self class menuPresenterClass new.
	stack := Stack new
		push: self menuPresenter;
		yourself
]

{ #category : #accessing }
SpMenuPresenterBuilder >> menuPresenter [
	^ menuPresenter
]

{ #category : #accessing }
SpMenuPresenterBuilder >> menuPresenter: anObject [
	menuPresenter := anObject
]

{ #category : #visiting }
SpMenuPresenterBuilder >> visitCommand: aCmCommand [

	aCmCommand 
		displayIn: stack top 
		do: [ :item | self fillItem: item with: aCmCommand ]
]

{ #category : #visiting }
SpMenuPresenterBuilder >> visitCommandGroup: aCmCommandsGroup [

	aCmCommandsGroup isRoot ifTrue: [ 
		super visitCommandGroup: aCmCommandsGroup.
		^ self ].
	
	aCmCommandsGroup 
		displayIn: stack top 
		do: [ :specGroupOrSubMenu |
			stack push: specGroupOrSubMenu.
			super visitCommandGroup: aCmCommandsGroup.
			stack pop ]
]

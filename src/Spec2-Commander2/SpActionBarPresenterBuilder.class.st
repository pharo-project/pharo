"
I build a ActionBarPresenter from a group of commands.

To do that, I visit recursively groups of command breadth first and simple add each command encountered to the action bar.
"
Class {
	#name : #SpActionBarPresenterBuilder,
	#superclass : #CmVisitor,
	#instVars : [
		'actionBarPresenter'
	],
	#category : #'Spec2-Commander2-Visitors'
}

{ #category : #accessing }
SpActionBarPresenterBuilder >> actionBarPresenter [
	^ actionBarPresenter
]

{ #category : #accessing }
SpActionBarPresenterBuilder >> actionBarPresenter: anObject [
	actionBarPresenter := anObject
]

{ #category : #initialization }
SpActionBarPresenterBuilder >> initialize [
	super initialize.
	self actionBarPresenter: SpActionBarPresenter new.
]

{ #category : #visiting }
SpActionBarPresenterBuilder >> visitCommand: aCmCommandEntry [ 
	aCmCommandEntry positionStrategy
		addButton: aCmCommandEntry asButtonPresenter
		toActionBar: self actionBarPresenter
]

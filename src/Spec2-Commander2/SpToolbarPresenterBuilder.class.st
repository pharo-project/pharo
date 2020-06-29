"
I build a ToolBarPresenter from a group of commands.

To do that, I visit recursively groups of command breadth first and simple add each command encountered to the tool bar.
"
Class {
	#name : #SpToolbarPresenterBuilder,
	#superclass : #CmVisitor,
	#instVars : [
		'toolbarPresenter'
	],
	#category : #'Spec2-Commander2-Visitors'
}

{ #category : #initialization }
SpToolbarPresenterBuilder >> initialize [
	super initialize.
	self toolbarPresenter: SpToolbarPresenter new.
]

{ #category : #accessing }
SpToolbarPresenterBuilder >> toolbarPresenter [
	^ toolbarPresenter
]

{ #category : #accessing }
SpToolbarPresenterBuilder >> toolbarPresenter: anObject [
	toolbarPresenter := anObject
]

{ #category : #visiting }
SpToolbarPresenterBuilder >> visitCommand: aCmCommandEntry [
	aCmCommandEntry positionStrategy
		addButton: aCmCommandEntry buildPresenter
		toToolbar: self toolbarPresenter
]

"
I build a ToolBarPresenter from a group of commands.

To do that, I visit recursively groups of command breadth first and simple add each command encountered to the tool bar.
"
Class {
	#name : #SpToolBarPresenterBuilder,
	#superclass : #CmVisitor,
	#instVars : [
		'toolbarPresenter'
	],
	#category : #'Spec2-Commander2-Visitors'
}

{ #category : #initialization }
SpToolBarPresenterBuilder >> initialize [
	super initialize.
	self toolbarPresenter: SpToolBarPresenter new.
]

{ #category : #accessing }
SpToolBarPresenterBuilder >> toolbarPresenter [
	^ toolbarPresenter
]

{ #category : #accessing }
SpToolBarPresenterBuilder >> toolbarPresenter: anObject [
	toolbarPresenter := anObject
]

{ #category : #visiting }
SpToolBarPresenterBuilder >> visitCommand: aCmCommandEntry [
	aCmCommandEntry positionStrategy
		addButton: (SpToolBarButton new
						label: aCmCommandEntry name;
						help: aCmCommandEntry description;
						icon: aCmCommandEntry icon;
						action: [ aCmCommandEntry execute ];
						yourself)
		toToolbar: self toolbarPresenter
]

"
A group of option buttons
"
Class {
	#name : #SpToolBarOptionButtonGroup,
	#superclass : #SpToolBarItemPresenter,
	#instVars : [
		'options'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolBarOptionButtonGroup class >> adapterName [

	^ #ToolBarOptionButtonGroupAdapter
]

{ #category : #accessing }
SpToolBarOptionButtonGroup >> addOption: anOption [

	(anOption isSelected not and: [ options isEmpty ])
		ifTrue: [ anOption beSelected ].
		
	(anOption isSelected and: [ options notEmpty ])
		ifTrue: [ options do: #beUnselected ].	

	options add: anOption.
	anOption buttonGroup: self.
]

{ #category : #initialization }
SpToolBarOptionButtonGroup >> initialize [
	super initialize.
	options := OrderedCollection new
]

{ #category : #accessing }
SpToolBarOptionButtonGroup >> options [
	^ options
]

{ #category : #accessing }
SpToolBarOptionButtonGroup >> options: anObject [
	options := anObject
]

{ #category : #refreshing }
SpToolBarOptionButtonGroup >> refresh [
	self shouldBeImplemented.
]

{ #category : #selection }
SpToolBarOptionButtonGroup >> unselectAll [
	
	options do: #beUnselected
]

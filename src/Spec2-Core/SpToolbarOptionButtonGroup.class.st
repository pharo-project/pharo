"
A group of option buttons
"
Class {
	#name : #SpToolbarOptionButtonGroup,
	#superclass : #SpToolbarItemPresenter,
	#instVars : [
		'options'
	],
	#category : #'Spec2-Core-Widgets-Toolbar'
}

{ #category : #specs }
SpToolbarOptionButtonGroup class >> adapterName [

	^ #ToolbarOptionButtonGroupAdapter
]

{ #category : #accessing }
SpToolbarOptionButtonGroup >> addOption: anOption [

	(anOption isSelected not and: [ options isEmpty ])
		ifTrue: [ anOption beSelected ].
		
	(anOption isSelected and: [ options notEmpty ])
		ifTrue: [ options do: #beUnselected ].	

	options add: anOption.
	anOption buttonGroup: self.
]

{ #category : #initialization }
SpToolbarOptionButtonGroup >> initialize [
	super initialize.
	options := OrderedCollection new
]

{ #category : #accessing }
SpToolbarOptionButtonGroup >> options [
	^ options
]

{ #category : #accessing }
SpToolbarOptionButtonGroup >> options: anObject [
	options := anObject
]

{ #category : #refreshing }
SpToolbarOptionButtonGroup >> refresh [
	self shouldBeImplemented.
]

{ #category : #selection }
SpToolbarOptionButtonGroup >> unselectAll [
	
	options do: #beUnselected
]

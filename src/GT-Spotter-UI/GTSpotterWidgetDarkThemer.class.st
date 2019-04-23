Class {
	#name : #GTSpotterWidgetDarkThemer,
	#superclass : #GTSpotterWidgetThemer,
	#category : #'GT-Spotter-UI-Theme'
}

{ #category : #values }
GTSpotterWidgetDarkThemer >> backgroundColor [
	"used as background color for all panes"
	^ Smalltalk ui theme lightBackgroundColor slightlyLighter
]

{ #category : #values }
GTSpotterWidgetDarkThemer >> borderColor [
	"color that is used for border and dividers of UI parts of Spotter
	for example divider between header and results or between preview and list"
	^ Smalltalk ui theme borderColor
]

{ #category : #values }
GTSpotterWidgetDarkThemer >> itemActionSelectedBackgroundColor [

	^ self itemSelectedBackgroundColor slightlyDarker
]

{ #category : #values }
GTSpotterWidgetDarkThemer >> itemIconAlphaValue [

	^ 0.5
]

{ #category : #values }
GTSpotterWidgetDarkThemer >> itemSelectedBackgroundColor [

	^ self backgroundColor slightlyDarker
]

{ #category : #values }
GTSpotterWidgetDarkThemer >> searchFieldTextColor [
	
	^ Color white slightlyDarker
]

{ #category : #values }
GTSpotterWidgetDarkThemer >> titleTextColor [

	^ self backgroundColor muchLighter
]

Class {
	#name : #GTSpotterWidgetDarkThemer,
	#superclass : #GTSpotterWidgetThemer,
	#category : #'GT-Spotter-Theme'
}

{ #category : #values }
GTSpotterWidgetDarkThemer >> backgroundColor [
	"used as background color for all panes"
	^ Color r: 0.25 g: 0.25 b: 0.25
]

{ #category : #values }
GTSpotterWidgetDarkThemer >> borderColor [
	"color that is used for border and dividers of UI parts of Spotter
	for example divider between header and results or between preview and list"
	^ self backgroundColor darker
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

Class {
	#name : #GTSpotterBrickDarkThemer,
	#superclass : #GLMBrickDarkThemer,
	#category : #'GT-Spotter-Theme'
}

{ #category : #'brick-rubric' }
GTSpotterBrickDarkThemer >> rubricBackgroundColor [

	^ self spotterThemer backgroundColor
]

{ #category : #'brick-scrollbar' }
GTSpotterBrickDarkThemer >> scrollbarColor [

	^ Color black alpha: 0.5
]

{ #category : #'brick-scrollpane' }
GTSpotterBrickDarkThemer >> scrollpaneBackgroundColor [

	^ self spotterThemer backgroundColor
]

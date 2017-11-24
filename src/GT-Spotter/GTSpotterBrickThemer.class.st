Class {
	#name : #GTSpotterBrickThemer,
	#superclass : #GLMBrickThemer,
	#category : #'GT-Spotter-Theme'
}

{ #category : #'brick-rubric' }
GTSpotterBrickThemer >> rubricBackgroundColor [

	^ self spotterThemer backgroundColor
]

{ #category : #'brick-scrollbar' }
GTSpotterBrickThemer >> scrollbarColor [

	^ Color lightGray alpha: 0.2
]

{ #category : #'brick-scrollpane' }
GTSpotterBrickThemer >> scrollpaneBackgroundColor [

	^ self spotterThemer backgroundColor
]

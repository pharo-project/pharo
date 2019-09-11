Extension { #name : #GLMBrickThemer }

{ #category : #'*GT-Spotter-UI' }
GLMBrickThemer >> spotterThemer [

	^ self
		registerModule: GTSpotterWidgetThemer new
		to: GTSpotterBrickThemer new
]

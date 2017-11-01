Extension { #name : #GLMBrickThemer }

{ #category : #'*GT-Spotter' }
GLMBrickThemer >> spotterThemer [

	^ self
		registerModule: GTSpotterWidgetThemer new
		to: GTSpotterBrickThemer new
]

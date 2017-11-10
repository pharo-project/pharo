Extension { #name : #GLMBrickDarkThemer }

{ #category : #'*GT-Spotter' }
GLMBrickDarkThemer >> spotterThemer [

	^ self
		registerModule: GTSpotterWidgetDarkThemer new
		to: GTSpotterBrickDarkThemer new
]

Extension { #name : #GLMBrickDarkThemer }

{ #category : #'*GT-Spotter-UI' }
GLMBrickDarkThemer >> spotterThemer [

	^ self
		registerModule: GTSpotterWidgetDarkThemer new
		to: GTSpotterBrickDarkThemer new
]

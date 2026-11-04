"
I am the color configurator that defined the colors for the pharo light theme
"
Class {
	#name : 'LightThemeColorConfigurator',
	#superclass : 'UIThemeColorConfigurator',
	#category : 'Morphic-Base-Theme',
	#package : 'Morphic-Base',
	#tag : 'Theme'
}

{ #category : 'colors' }
LightThemeColorConfigurator >> balloonBackgroundColor [

	^ (Color r: 1.0 g: 1.0 b: 0.71 alpha: 1.0)
]

{ #category : 'colors' }
LightThemeColorConfigurator >> baseColor [

	^ Color r: 210 g: 210 b: 210 range: 255
]

{ #category : 'colors' }
LightThemeColorConfigurator >> basePassiveBackgroundColor [

	^ Color r: 245 g: 245 b: 245 range: 255
]

{ #category : 'colors' }
LightThemeColorConfigurator >> baseSelectionColor [

	^ Color r: 97 g: 163 b: 225 range: 255
]

{ #category : 'colors' }
LightThemeColorConfigurator >> darkBaseColor [

	^ Color r: 200 g: 200 b: 200 range: 255
]

{ #category : 'colors' }
LightThemeColorConfigurator >> ghostTextColor [

	^ Color darkGray
]

{ #category : 'colors' }
LightThemeColorConfigurator >> lightBaseColor [

	^ Color r: 230 g: 230 b: 230 range: 255
]

{ #category : 'colors' }
LightThemeColorConfigurator >> veryLightSelectionColor [

	^ self lightSelectionColor muchLighter
]

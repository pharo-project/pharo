"
DarkBlue theme configurator
"
Class {
	#name : 'DarkBlueThemeConfigurator',
	#superclass : 'DarkThemeColorConfigurator',
	#category : 'DarkBlueTheme',
	#package : 'DarkBlueTheme'
}

{ #category : 'colors' }
DarkBlueThemeConfigurator >> backgroundColor [

	^ Color fromHexString: '1B2536'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> baseColor [

	^ Color fromHexString: '212D41'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> basePassiveBackgroundColor [

	^ Color fromHexString: '2A384F'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> baseSelectionColor [

	^ Color fromHexString: '5B9BFF'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> borderColor [

	^ Color fromHexString: '364660'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> buttonColor [

	^ Color fromHexString: '324260'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> caretColor [

	^ Color fromHexString: '8FBCFF'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> darkBaseColor [

	^ Color fromHexString: '172030'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> desktopColor [

	^ Color fromHexString: '141B27'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> disabledColor [

	^ Color fromHexString: '8B98AD'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> growlBorderColorFor: aGrowlMorph [

	^ self borderColor
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> growlFillColorFor: aGrowlMorph [

	^ self lightBackgroundColor
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> lightBackgroundColor [

	^ Color fromHexString: '2A384F'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> lightSelectionColor [

	^ self selectionColor
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> lineNumberColor [

	^ Color fromHexString: '8B98AD'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> menuBorderColor [

	^ Color fromHexString: '415470'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> scrollbarColor [

	^ Color fromHexString: '43556F'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> secondarySelectionColor [

	^ Color fromHexString: '2F3E57'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> selectionColor [

	^ Color fromHexString: '264576'
]

{ #category : 'colors' }
DarkBlueThemeConfigurator >> textColor [

	^ Color fromHexString: 'D6DEEA'
]

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
LightThemeColorConfigurator >> backgroundColor [

	^ Color fromHexString: 'F4F5F7'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> balloonBackgroundColor [

	^ Color fromHexString: '2B2F37'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> balloonTextColor [

	^ Color fromHexString: 'F1F3F5'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> baseColor [

	^ Color fromHexString: 'ECEEF1'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> basePassiveBackgroundColor [

	^ Color fromHexString: 'F0F1F4'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> baseSelectionColor [

	^ Color fromHexString: '2F6FE0'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> borderColor [

	^ Color fromHexString: 'CACED5'
]

{ #category : 'buttons' }
LightThemeColorConfigurator >> buttonColor [

	^ self lightBaseColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> caretColor [

	^ Color fromHexString: '2F6FE0'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> darkBaseColor [

	^ Color fromHexString: 'D8DBE1'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> desktopColor [

	^ Color fromHexString: 'DDE0E5'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> disabledColor [

	^ Color fromHexString: '6B727D'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> disabledLabelColor [

	^ Color fromHexString: '6B727D'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> dropListNormalFillStyleFor: anObject [

	^ SolidFillStyle color: self backgroundColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> enabledColor [

	^ self textColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> ghostTextColor [

	^ Color fromHexString: '6B727D'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> lightBackgroundColor [

	^ Color fromHexString: 'E3E5EA'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> lightBaseColor [

	^ self backgroundColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> lightSelectionColor [

	^ Color fromHexString: 'CFE0FC'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> lineNumberColor [

	^ Color fromHexString: '6B727D'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> listTextColor [

	^ self textColor
]

{ #category : 'menus' }
LightThemeColorConfigurator >> menuBorderColor [

	^ self borderColor
]

{ #category : 'menus' }
LightThemeColorConfigurator >> menuColor [

	^ self backgroundColor
]

{ #category : 'menus' }
LightThemeColorConfigurator >> menuItemDisabledTextColorFor: aMenuItem [

	^ self disabledColor
]

{ #category : 'menus' }
LightThemeColorConfigurator >> menuSelectionColor [

	^ self baseSelectionColor
]

{ #category : 'menus' }
LightThemeColorConfigurator >> menuTitleColor [

	^ self menuColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> mouseOverColor [

	^ self lightBackgroundColor
]

{ #category : 'scrollbars' }
LightThemeColorConfigurator >> scrollbarColor [

	^ Color fromHexString: 'C4C9D0'
]

{ #category : 'scrollbars' }
LightThemeColorConfigurator >> scrollbarImageColorFor: aScrollbar [

	^ self disabledColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> scrollbarNormalFillStyleFor: aScrollbar [

	"Return the normal scrollbar fillStyle for the given scrollbar."

	^ SolidFillStyle color: self backgroundColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> secondarySelectionColor [

	^ Color fromHexString: 'E3E6EA'
]

{ #category : 'custom' }
LightThemeColorConfigurator >> selectedTabBorderColor [

	^ self baseSelectionColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> selectionColor [

	^ Color fromHexString: 'CFE0FC'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> selectionTextColor [

	^ self textColor
]

{ #category : 'taskbar' }
LightThemeColorConfigurator >> taskbarItemLabelColorForCollapsed: anObject [

	^ self disabledColor
]

{ #category : 'taskbar' }
LightThemeColorConfigurator >> taskbarItemLabelColorForExpanded: anObject [

	^ self textColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> textColor [

	^ Color fromHexString: '2A2F36'
]

{ #category : 'colors' }
LightThemeColorConfigurator >> unfocusedSelectionColor [

	^ self secondarySelectionColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> veryLightSelectionColor [

	^ self lightSelectionColor muchLighter
]

{ #category : 'windows' }
LightThemeColorConfigurator >> windowActiveLabelFillStyleFor: anObject [

	^ self textColor
]

{ #category : 'windows' }
LightThemeColorConfigurator >> windowColor [

	^ self baseColor
]

{ #category : 'colors' }
LightThemeColorConfigurator >> worldMainDockingBarColorFor: anObject [

	^ self baseColor
]

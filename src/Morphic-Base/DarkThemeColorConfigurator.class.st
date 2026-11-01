"
I am the color configurator that defined the colors for the pharo dark theme
"
Class {
	#name : 'DarkThemeColorConfigurator',
	#superclass : 'UIThemeColorConfigurator',
	#category : 'Morphic-Base-Theme',
	#package : 'Morphic-Base',
	#tag : 'Theme'
}

{ #category : 'colors' }
DarkThemeColorConfigurator >> backgroundColor [

	^ Color fromHexString: '22252B'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> badgeColor [

	^ Color red alpha: 0.5
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> badgeTextColor [

	^ self textColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> balloonBackgroundColor [

	^ self menuColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> baseColor [

	^ Color fromHexString: '282C33'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> basePassiveBackgroundColor [

	^ Color fromHexString: '30343C'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> baseSelectionColor [

	^ Color fromHexString: '4C8DFF'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> borderColor [

	^ Color fromHexString: '3C414B'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> buttonColor [

	^ Color fromHexString: '3B4049'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> caretColor [

	^ Color fromHexString: '7FB0FF'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> classExtensionColor [

	^ Color fromHexString: 'A3AAB5'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> dangerTextColor [

	^ Color r: 239 g: 83 b: 80 range: 255
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> darkBaseColor [

	^ Color fromHexString: '1D2025'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> desktopColor [

	^ Color fromHexString: '1A1D22'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> diffAdditionalLineBackgroundColor [

	^ Color r: 0.0 g: 0.76 b: 0.0 alpha: 0.2
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> diffEdgeColor [

	^ Color white alpha: 0.5
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> diffModificatedLineBackgroundColor [

	^ Color gray alpha: 0.2
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> diffRemovedLinesBackgroundColor [

	^ Color r: 0.84 g: 0.0 b: 0.0 alpha: 0.2
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> disabledColor [

	^ Color fromHexString: '8A919C'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> disabledLabelColor [

	^ self disabledColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> dropListNormalFillStyleFor: aDropList [

	^ self backgroundColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> dropListNormalListFillStyleFor: aDropList [

	^ self backgroundColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> enabledColor [

	^ self textColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> errorPopperBackgroundColor [

	^ Color red muchDarker
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> highlightTextColor [
	"I am a color used to have a text color that is more visible than the classic text color to do some highlights."

	^ Color fromHexString: 'FF6E40'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> lessConspicuousColorFrom: aColor [

	^ aColor slightlyLighter
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> lightBackgroundColor [

	^ Color fromHexString: '30343C'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> lightBaseColor [

	^ self lightBackgroundColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> lineNumberColor [

	^ Color fromHexString: '8A919C'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> listNormalFillStyleFor: aList [

	^ self backgroundColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> listTextColor [

	^ self textColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> menuBorderColor [

	^ Color fromHexString: '474D58'
]

{ #category : 'menus' }
DarkThemeColorConfigurator >> menuColor [

	^ self lightBackgroundColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> menuItemDisabledTextColorFor: aMenuItem [

	^ self disabledColor
]

{ #category : 'menus' }
DarkThemeColorConfigurator >> menuSelectionColor [

	^ Color fromHexString: '2F6FE0'
]

{ #category : 'menus' }
DarkThemeColorConfigurator >> menuTitleColor [

	^ self menuColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> mouseOverColor [

	^ self lightBackgroundColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> paginatorSelectionColor [

	^ self lightBaseColor alpha: 0.8
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> popoverButtonColor [

	^ self lightBaseColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> scrollbarColor [

	^ Color fromHexString: '4A505C'
]

{ #category : 'scrollbars' }
DarkThemeColorConfigurator >> scrollbarImageColorFor: aScrollbar [

	^ self disabledColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> scrollbarNormalFillStyleFor: aScrollbar [

	"Return the normal scrollbar fillStyle for the given scrollbar."

	^ SolidFillStyle color: self backgroundColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> secondarySelectionColor [

	^ Color fromHexString: '3C414B'
]

{ #category : 'custom' }
DarkThemeColorConfigurator >> selectedTabBorderColor [

	^ self baseSelectionColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> selectionColor [

	^ Color fromHexString: '27416A'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> selectionTextColor [

	^ Color fromHexString: 'FFFFFF'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> sliderNormalFillStyleFor: aSlider [

	^ self backgroundColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> spotlightWindowColor [

	^ self windowColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> successTextColor [

	^ Color green darker darker darker
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> taskbarItemLabelColorForCollapsed: aButton [

	^ self disabledColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> taskbarItemLabelColorForExpanded: aButton [

	^ self textColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> textColor [

	^ Color fromHexString: 'D5D9E0'
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> textColorForNonStandardBackground [

	^ Color black
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> textEditorNormalFillStyleFor: aTextEditor [

	^ self backgroundColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> unfocusedSelectionColor [

	^ self secondarySelectionColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> veryLightSelectionColor [

	^ self lightSelectionColor muchLighter
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> warningTextColor [

	^ Color yellow darker darker
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> windowActiveLabelFillStyleFor: aWindow [

	^ self textColor
]

{ #category : 'windows' }
DarkThemeColorConfigurator >> windowColor [

	^ self baseColor
]

{ #category : 'colors' }
DarkThemeColorConfigurator >> worldMainDockingBarColorFor: anObject [

	^ self darkBaseColor
]

"
I am the color configurator that defined the colors for the pharo dark theme
"
Class {
	#name : #DarkThemeColorConfigurator,
	#superclass : #UIThemeColorConfigurator,
	#category : #'Polymorph-Widgets-Themes'
}

{ #category : #colors }
DarkThemeColorConfigurator >> backgroundColor [

	^ Color r: 0.130 g: 0.130 b: 0.130
]

{ #category : #colors }
DarkThemeColorConfigurator >> badgeColor [

	^ Color red alpha: 0.5
]

{ #category : #colors }
DarkThemeColorConfigurator >> badgeTextColor [

	^ self textColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> balloonBackgroundColor [

	^ self darkBaseColor lighter
]

{ #category : #colors }
DarkThemeColorConfigurator >> baseColor [

	^ Color veryDarkGray
]

{ #category : #colors }
DarkThemeColorConfigurator >> basePassiveBackgroundColor [

	^ Color darkGray
]

{ #category : #colors }
DarkThemeColorConfigurator >> baseSelectionColor [

	^ Color r: 97 g: 163 b: 225 range: 255
]

{ #category : #colors }
DarkThemeColorConfigurator >> borderColor [

	^ Color darkGray darker darker darker
]

{ #category : #colors }
DarkThemeColorConfigurator >> buttonColor [

	^ self backgroundColor lighter lighter
]

{ #category : #colors }
DarkThemeColorConfigurator >> caretColor [

	^ Color lightBlue
]

{ #category : #colors }
DarkThemeColorConfigurator >> classExtensionColor [

	^ Color gray lighter lighter
]

{ #category : #colors }
DarkThemeColorConfigurator >> dangerTextColor [

	^ Color r: 239 g: 83 b: 80 range: 255
]

{ #category : #colors }
DarkThemeColorConfigurator >> darkBaseColor [

	^ Color veryDarkGray
]

{ #category : #colors }
DarkThemeColorConfigurator >> desktopColor [

	^ Color darkGray darker
]

{ #category : #colors }
DarkThemeColorConfigurator >> diffAdditionalLineBackgroundColor [
	
	^ Color r: 0.0 g: 0.76 b: 0.0 alpha: 0.2
]

{ #category : #colors }
DarkThemeColorConfigurator >> diffEdgeColor [

	^ Color white alpha: 0.5
]

{ #category : #colors }
DarkThemeColorConfigurator >> diffModificatedLineBackgroundColor [

	^ Color gray alpha: 0.2
]

{ #category : #colors }
DarkThemeColorConfigurator >> diffRemovedLinesBackgroundColor [
		
	^ Color r: 0.84 g: 0.0 b: 0.0 alpha: 0.2
]

{ #category : #colors }
DarkThemeColorConfigurator >> disabledColor [

	^ Color gray lighter
]

{ #category : #colors }
DarkThemeColorConfigurator >> disabledLabelColor [

	^ Color r: 0.7673509286412512
		  g: 0.7673509286412512
		  b: 0.7673509286412512
		  alpha: 0.7647058823529411
]

{ #category : #colors }
DarkThemeColorConfigurator >> dropListNormalFillStyleFor: aDropList [

	^ self backgroundColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> dropListNormalListFillStyleFor: aDropList [

	^ self backgroundColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> enabledColor [

	^ self textColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> errorPopperBackgroundColor [

	^ Color red muchDarker
]

{ #category : #colors }
DarkThemeColorConfigurator >> lessConspicuousColorFrom: aColor [

	^ aColor slightlyLighter
]

{ #category : #colors }
DarkThemeColorConfigurator >> lightBackgroundColor [

	^ Color r: 0.218 g: 0.217 b: 0.217
]

{ #category : #colors }
DarkThemeColorConfigurator >> lightBaseColor [

	^ Color darkGray
]

{ #category : #colors }
DarkThemeColorConfigurator >> lineNumberColor [

	^ Color gray lighter lighter
]

{ #category : #colors }
DarkThemeColorConfigurator >> listNormalFillStyleFor: aList [

	^ self backgroundColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> listTextColor [

	^ self textColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> menuBorderColor [

	^ self borderColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> menuItemDisabledTextColorFor: aMenuItem [

	^ Color lightGray
]

{ #category : #colors }
DarkThemeColorConfigurator >> mouseOverColor [

	^ self lightBackgroundColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> paginatorSelectionColor [

	^ self lightBaseColor alpha: 0.8
]

{ #category : #colors }
DarkThemeColorConfigurator >> popoverButtonColor [

	^ self lightBaseColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> scrollbarColor [

	^ self buttonColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> scrollbarNormalFillStyleFor: aScrollbar [

	"Return the normal scrollbar fillStyle for the given scrollbar."

	^ SolidFillStyle color: self backgroundColor lighter
]

{ #category : #colors }
DarkThemeColorConfigurator >> secondarySelectionColor [

	^ Color r: 0.31 g: 0.31 b: 0.36 alpha: 1.0
]

{ #category : #colors }
DarkThemeColorConfigurator >> selectionColor [

	^ Color r: 16r07 g: 16r63 b: 16rAD range: 255
]

{ #category : #colors }
DarkThemeColorConfigurator >> selectionTextColor [

	^ Color r: 16rE0 g: 16rE2 b: 16rE4 range: 255
]

{ #category : #colors }
DarkThemeColorConfigurator >> sliderNormalFillStyleFor: aSlider [

	^ self backgroundColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> spotlightWindowColor [

	^ self windowColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> successTextColor [

	^ Color green darker darker darker
]

{ #category : #colors }
DarkThemeColorConfigurator >> taskbarItemLabelColorForCollapsed: aButton [

	^ Color lightGray
]

{ #category : #colors }
DarkThemeColorConfigurator >> taskbarItemLabelColorForExpanded: aButton [

	^ Color white
]

{ #category : #colors }
DarkThemeColorConfigurator >> textColor [

	^ Color white
]

{ #category : #colors }
DarkThemeColorConfigurator >> textColorForNonStandardBackground [

	^ Color black
]

{ #category : #colors }
DarkThemeColorConfigurator >> textEditorNormalFillStyleFor: aTextEditor [

	^ self backgroundColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> unfocusedSelectionColor [

	^ self secondarySelectionColor
]

{ #category : #colors }
DarkThemeColorConfigurator >> veryLightSelectionColor [

	^ self lightSelectionColor muchLighter
]

{ #category : #colors }
DarkThemeColorConfigurator >> warningTextColor [

	^ Color yellow darker darker
]

{ #category : #colors }
DarkThemeColorConfigurator >> windowActiveLabelFillStyleFor: aWindow [

	^ self textColor
]

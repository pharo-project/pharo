"
I am a class that is in charge of return the colors for the UITheme. You need to subclass me to use me with a theme. For example PharoDarkTheme uses DarkThemeColorConfigurator.

So, if you want to create your own theme, you need to subclass me and redefine my abstract methods. You can also override any method to change the colors.
"
Class {
	#name : 'UIThemeColorConfigurator',
	#superclass : 'Object',
	#instVars : [
		'themeSettings'
	],
	#category : 'Morphic-Base-Theme',
	#package : 'Morphic-Base',
	#tag : 'Theme'
}

{ #category : 'colors' }
UIThemeColorConfigurator >> backgroundColor [

	^ Color white
]

{ #category : 'colors' }
UIThemeColorConfigurator >> badgeColor [

	^ Color red
]

{ #category : 'colors' }
UIThemeColorConfigurator >> badgeTextColor [

	^ Color white
]

{ #category : 'colors' }
UIThemeColorConfigurator >> balloonBackgroundColor [

	^ self lightBaseColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> balloonTextColor [

	^ self textColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> baseColor [

	^ self subclassResponsibility
]

{ #category : 'colors' }
UIThemeColorConfigurator >> baseColorFor: anObject [

	^ self baseColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> basePassiveBackgroundColor [

	^ self subclassResponsibility
]

{ #category : 'scrollbars' }
UIThemeColorConfigurator >> baseScrollbarColorFor: aScrollbar [

	^ aScrollbar lastPaneColor ifNil: [ self scrollbarColor ]
]

{ #category : 'colors' }
UIThemeColorConfigurator >> baseSelectionColor [

	^ self subclassResponsibility
]

{ #category : 'colors' }
UIThemeColorConfigurator >> borderColor [

	^ Color gray
]

{ #category : 'buttons' }
UIThemeColorConfigurator >> buttonColor [

	^ themeSettings buttonColor
]

{ #category : 'buttons' }
UIThemeColorConfigurator >> buttonColorFor: aButton [

	^ themeSettings standardColorsOnly
		ifTrue: [ self buttonColor ]
		ifFalse: [ aButton colorToUse ]
]

{ #category : 'buttons' }
UIThemeColorConfigurator >> buttonColouredMiddleColor [

	"Return the middle area colour for a coloured button fillStyle."

	^ Color r: 102 g: 127 b: 168 range: 255
]

{ #category : 'buttons' }
UIThemeColorConfigurator >> buttonPlainMiddleColor [

	"Return the middle area colour for a plain button fillStyle."

	^ Color r: 211 g: 211 b: 211 range: 255
]

{ #category : 'colors' }
UIThemeColorConfigurator >> calypsoFrozenItemColor [

	"For Pharo 6 compatibilit reason it is not separate overrides in concrete theme classes.
	But this trick still allows override this color on external themes.
	Idea to only cover default ones which are white and dark"

	^ self backgroundColor lightness > 0.3
		ifTrue: [ Color blue muchDarker ]
		ifFalse: [ Color cyan ]
]

{ #category : 'colors' }
UIThemeColorConfigurator >> caretColor [

	^ Color blue
]

{ #category : 'colors' }
UIThemeColorConfigurator >> classExtensionColor [

	^ Color gray darker
]

{ #category : 'colors' }
UIThemeColorConfigurator >> dangerBackgroundColor [

	^ self dangerTextColor darker darker
]

{ #category : 'colors' }
UIThemeColorConfigurator >> dangerTextColor [

	^ Color red darker darker
]

{ #category : 'colors' }
UIThemeColorConfigurator >> darkBaseColor [

	^ self subclassResponsibility
]

{ #category : 'colors' }
UIThemeColorConfigurator >> desktopColor [

	^ Color r: 240 g: 240 b: 240 range: 255
]

{ #category : 'colors' }
UIThemeColorConfigurator >> diffAdditionalLineBackgroundColor [

	^ Color green alpha: 0.2
]

{ #category : 'colors' }
UIThemeColorConfigurator >> diffAdditionalTextInLineBackgroundColor [

	^ self diffAdditionalLineBackgroundColor alpha: 0.4
]

{ #category : 'colors' }
UIThemeColorConfigurator >> diffEdgeColor [

	^ Color gray alpha: 0.3
]

{ #category : 'colors' }
UIThemeColorConfigurator >> diffJoinColor [

	^ Color paleBlue duller
]

{ #category : 'colors' }
UIThemeColorConfigurator >> diffMapColor [

	^ self backgroundColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> diffModificatedLineBackgroundColor [

	^ Color yellow alpha: 0.2
]

{ #category : 'colors' }
UIThemeColorConfigurator >> diffRemovalHighlightColor [

	^ self diffRemovedLinesBackgroundColor alpha: 0.4
]

{ #category : 'colors' }
UIThemeColorConfigurator >> diffRemovedLinesBackgroundColor [

	^ Color red alpha: 0.2
]

{ #category : 'colors' }
UIThemeColorConfigurator >> disabledColor [

	^ Color gray
]

{ #category : 'colors' }
UIThemeColorConfigurator >> disabledLabelColor [

	^ Color r: 0.4115347018572825
		  g: 0.4115347018572825
		  b: 0.4115347018572825
		  alpha: 1.0
]

{ #category : 'colors' }
UIThemeColorConfigurator >> disabledTextColor [

	^ self disabledColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> dropListNormalFillStyleFor: anObject [

	^ SolidFillStyle color: Color white
]

{ #category : 'colors' }
UIThemeColorConfigurator >> dropListNormalListFillStyleFor: anObject [

	^ Color white
]

{ #category : 'colors' }
UIThemeColorConfigurator >> enabledColor [

	^ Color black
]

{ #category : 'colors' }
UIThemeColorConfigurator >> errorPopperBackgroundColor [

	^ Color paleRed
]

{ #category : 'colors' }
UIThemeColorConfigurator >> ghostTextColor [

	^ Color lightGray
]

{ #category : 'colors' }
UIThemeColorConfigurator >> growlBorderColorFor: aGrowlMorph [

	^ Color gray
]

{ #category : 'colors' }
UIThemeColorConfigurator >> growlContentsColorFor: aGrowlMorph [

	^ Color white
]

{ #category : 'colors' }
UIThemeColorConfigurator >> growlFillColorFor: aGrowlMorph [

	^ Color darkGray alpha: 0.5
]

{ #category : 'colors' }
UIThemeColorConfigurator >> growlLabelColorFor: aGrowlMorph [

	^ Color white twiceDarker
]

{ #category : 'colors' }
UIThemeColorConfigurator >> highlightTextColor [
	"I am a color used to have a text color that is more visible than the classic text color to do some highlights."

	^ Color fromHexString: '7C4DFF'
]

{ #category : 'colors' }
UIThemeColorConfigurator >> lessConspicuousColorFrom: aColor [

	^ aColor alphaMixed: 0.5 with: Color white
]

{ #category : 'colors' }
UIThemeColorConfigurator >> lightBackgroundColor [

	^ Color veryLightGray muchLighter
]

{ #category : 'colors' }
UIThemeColorConfigurator >> lightBaseColor [

	^ self subclassResponsibility
]

{ #category : 'colors' }
UIThemeColorConfigurator >> lightColorFor: aButton [

	^ self lightBaseColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> lightSelectionColor [

	^ Color r: 175 g: 213 b: 250 range: 255
]

{ #category : 'colors' }
UIThemeColorConfigurator >> lineNumberColor [

	^ Color gray muchDarker
]

{ #category : 'colors' }
UIThemeColorConfigurator >> listBackgroundColor [

	^ self backgroundColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> listNormalFillStyleFor: anObject [

	^ Color white
]

{ #category : 'colors' }
UIThemeColorConfigurator >> listTextColor [

	^ Color black
]

{ #category : 'menus' }
UIThemeColorConfigurator >> menuBorderColor [

	^ themeSettings menuBorderColor
]

{ #category : 'menus' }
UIThemeColorConfigurator >> menuColor [

	^ themeSettings menuColor
]

{ #category : 'menus' }
UIThemeColorConfigurator >> menuItemDisabledTextColorFor: aMenuItem [

	^ (aMenuItem color luminance - aMenuItem owner color luminance) abs < 0.3
		ifTrue: [ aMenuItem owner color contrastingBlackAndWhiteColor muchDarker ]
		ifFalse: [ aMenuItem owner color muchDarker ]
]

{ #category : 'menus' }
UIThemeColorConfigurator >> menuKeyboardFocusColor [

	^ themeSettings menuKeyboardFocusColor
]

{ #category : 'menus' }
UIThemeColorConfigurator >> menuSelectionColor [

	^ themeSettings menuSelectionColor
]

{ #category : 'menus' }
UIThemeColorConfigurator >> menuShadowColor [

	^ themeSettings menuShadowColor
]

{ #category : 'menus' }
UIThemeColorConfigurator >> menuTitleColor [

	^ themeSettings menuTitleColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> modalBackdropColor [

	"This method should probably be moved to Pharo later."

	^ Color black alpha: 0.2
]

{ #category : 'colors' }
UIThemeColorConfigurator >> mouseOverColor [

	^ self backgroundColor darker darker
]

{ #category : 'colors' }
UIThemeColorConfigurator >> paginatorSelectionColor [

	^ self darkBaseColor alpha: 0.8
]

{ #category : 'colors' }
UIThemeColorConfigurator >> patchworkUIThemeColorFor: anObject [

	^ themeSettings windowColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> popoverButtonColor [

	^ self darkBaseColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> progressBarColor [

	^ themeSettings progressBarColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> resizerGripNormalFillStyleFor: aResizer [

	"Return the normal fillStyle for the given resizer.
	For the moment, answer a transparent colour for no drawing,
	non transparent to draw as normal."

	^ Color transparent
]

{ #category : 'scrollbars' }
UIThemeColorConfigurator >> scrollbarColor [

	^ themeSettings scrollbarColor
]

{ #category : 'scrollbars' }
UIThemeColorConfigurator >> scrollbarColorFor: aScrollbar [

	^ themeSettings standardColorsOnly
		ifTrue: [ themeSettings scrollbarColor]
		ifFalse: [ aScrollbar lastPaneColor ifNil: [ Color white ] ]
]

{ #category : 'scrollbars' }
UIThemeColorConfigurator >> scrollbarImageColorFor: aScrollbar [

	^ (self baseScrollbarColorFor: aScrollbar) darker
]

{ #category : 'scrollbars' }
UIThemeColorConfigurator >> scrollbarNormalFillStyleFor: aScrollbar [
	"Return the normal scrollbar fillStyle for the given scrollbar."

	^ 
	Color r: 245 g: 245 b: 245 range: 255
]

{ #category : 'colors' }
UIThemeColorConfigurator >> secondarySelectionColor [

	^ self selectionColor lighter lighter lighter lighter
]

{ #category : 'colors' }
UIThemeColorConfigurator >> selectedItemListTextColor [

	^ self textColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> selectionBarColor [

	^ themeSettings selectionBarColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> selectionColor [

	^ themeSettings selectionColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> selectionTextColor [

	^ themeSettings selectionTextColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> sliderNormalFillStyleFor: anObject [

	^ SolidFillStyle color: Color white
]

{ #category : 'colors' }
UIThemeColorConfigurator >> spotlightWindowColor [

	^ self selectionColor muchDarker
]

{ #category : 'colors' }
UIThemeColorConfigurator >> subgroupColorFrom: paneColor [

	^ self lightColorFor: paneColor "self class baseColor"
]

{ #category : 'colors' }
UIThemeColorConfigurator >> successBackgroundColor [

	^ self successTextColor lighter lighter
]

{ #category : 'colors' }
UIThemeColorConfigurator >> successTextColor [

	^ Color r: 0 g: 0.5 b: 0
]

{ #category : 'taskbar' }
UIThemeColorConfigurator >> taskbarActiveButtonColorFor: anObject [

	^ self scrollbarColor alphaMixed: 0.7 with: Color white
]

{ #category : 'taskbar' }
UIThemeColorConfigurator >> taskbarButtonColorFor: aButton [

	^ self windowColorFor: aButton model
]

{ #category : 'taskbar' }
UIThemeColorConfigurator >> taskbarButtonLabelColorFor: aButton [

	^ aButton model isActive
		ifTrue: [ Color black ]
		ifFalse: [ Color gray darker ]
]

{ #category : 'taskbar' }
UIThemeColorConfigurator >> taskbarItemLabelColorForCollapsed: anObject [

	^ Color darkGray
]

{ #category : 'taskbar' }
UIThemeColorConfigurator >> taskbarItemLabelColorForExpanded: anObject [

	^ Color black
]

{ #category : 'taskbar' }
UIThemeColorConfigurator >> taskbarMinimizedButtonColorFor: anObject [

	^ Color transparent
]

{ #category : 'colors' }
UIThemeColorConfigurator >> textColor [

	^ Color black
]

{ #category : 'colors' }
UIThemeColorConfigurator >> textColorForNonStandardBackground [

	^ self textColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> textEditorNormalFillStyleFor: anObject [

	^ Color white
]

{ #category : 'colors' }
UIThemeColorConfigurator >> textFieldNormalFillStyleFor: aTextField [

	"Return the normal fillStyle for the given text field."

	^ Color white
]

{ #category : 'accessing' }
UIThemeColorConfigurator >> themeSettings [

	^ themeSettings
]

{ #category : 'accessing' }
UIThemeColorConfigurator >> themeSettings: anObject [

	themeSettings := anObject
]

{ #category : 'basic-colors' }
UIThemeColorConfigurator >> treeLineColorsFrom: aColor [

	^ { aColor. Color transparent }
]

{ #category : 'colors' }
UIThemeColorConfigurator >> unfocusedSelectionColor [

	^ themeSettings unfocusedSelectionColor
]

{ #category : 'colors' }
UIThemeColorConfigurator >> urlColor [

	^ Color fromHexString: '03A9F4'
]

{ #category : 'colors' }
UIThemeColorConfigurator >> veryLightSelectionColor [

	^ self subclassResponsibility
]

{ #category : 'colors' }
UIThemeColorConfigurator >> warningBackgroundColor [

	^ Color yellow
]

{ #category : 'colors' }
UIThemeColorConfigurator >> warningTextColor [

	^ Color yellow muchDarker
]

{ #category : 'windows' }
UIThemeColorConfigurator >> windowActiveLabelFillStyleFor: anObject [

	^ Color black
]

{ #category : 'windows' }
UIThemeColorConfigurator >> windowColor [

	^ themeSettings windowColor
]

{ #category : 'windows' }
UIThemeColorConfigurator >> windowColorFor: anObject [

	^ themeSettings windowColor
]

{ #category : 'windows' }
UIThemeColorConfigurator >> windowEdgeNormalFillStyleFor: anEdgeGrip [

	"Return the normal window edge fillStyle for the given edge grip."

	^ Color transparent
]

{ #category : 'windows' }
UIThemeColorConfigurator >> windowShadowColor [

	^ Color gray
]

{ #category : 'colors' }
UIThemeColorConfigurator >> worldMainDockingBarColorFor: anObject [

	^ themeSettings derivedMenuColor
]

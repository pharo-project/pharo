"
I am a color palette. I cache the colors that are returned by the color configurator in a dictionary.
"
Class {
	#name : 'UIThemePalette',
	#superclass : 'Object',
	#instVars : [
		'colorConfigurator',
		'paletteDictionary'
	],
	#category : 'Morphic-Base-Theme',
	#package : 'Morphic-Base',
	#tag : 'Theme'
}

{ #category : 'colors' }
UIThemePalette >> backgroundColor [

	^ paletteDictionary at: #backgroundColor ifAbsentPut: [ colorConfigurator backgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> badgeColor [

	^ paletteDictionary at: #badgeColor ifAbsentPut: [ colorConfigurator badgeColor ]
]

{ #category : 'colors' }
UIThemePalette >> badgeTextColor [

	^ paletteDictionary at: #badgeTextColor ifAbsentPut: [ colorConfigurator badgeTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> balloonBackgroundColor [

	^ paletteDictionary at: #balloonBackgroundColor ifAbsentPut: [ colorConfigurator balloonBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> balloonTextColor [

	^ paletteDictionary at: #balloonTextColor ifAbsentPut: [ colorConfigurator balloonTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> baseColor [

	^ paletteDictionary at: #baseColor ifAbsentPut: [ colorConfigurator baseColor ]
]

{ #category : 'colors' }
UIThemePalette >> baseColorFor: anObject [

	^ paletteDictionary at: #baseColorFor: ifAbsentPut: [ colorConfigurator baseColorFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> basePassiveBackgroundColor [

	^ paletteDictionary at: #basePassiveBackgroundColor ifAbsentPut: [ colorConfigurator basePassiveBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> baseScrollbarColorFor: aScrollbar [

	^ paletteDictionary at: #baseScrollbarColorFor: ifAbsentPut: [ colorConfigurator baseScrollbarColorFor: aScrollbar ]
]

{ #category : 'colors' }
UIThemePalette >> baseSelectionColor [

	^ paletteDictionary at: #baseSelectionColor ifAbsentPut: [ colorConfigurator baseSelectionColor ]
]

{ #category : 'colors' }
UIThemePalette >> borderColor [

	^ paletteDictionary at: #borderColor ifAbsentPut: [ colorConfigurator borderColor ]
]

{ #category : 'colors' }
UIThemePalette >> buttonColor [

	^ paletteDictionary at: #buttonColor ifAbsentPut: [ colorConfigurator buttonColor ]
]

{ #category : 'colors' }
UIThemePalette >> buttonColorFor: aButton [

	^ paletteDictionary at: #buttonColorFor: ifAbsentPut: [ colorConfigurator buttonColorFor: aButton ]
]

{ #category : 'colors' }
UIThemePalette >> buttonColouredMiddleColor [

	^ paletteDictionary at: #buttonColouredMiddleColor ifAbsentPut: [ colorConfigurator buttonColouredMiddleColor ]
]

{ #category : 'colors' }
UIThemePalette >> buttonPlainMiddleColor [

	^ paletteDictionary at: #buttonPlainMiddleColor ifAbsentPut: [ colorConfigurator buttonPlainMiddleColor ]
]

{ #category : 'colors' }
UIThemePalette >> calypsoFrozenItemColor [

	^ paletteDictionary at: #calypsoFrozenItemColor ifAbsentPut: [ colorConfigurator calypsoFrozenItemColor ]
]

{ #category : 'colors' }
UIThemePalette >> caretColor [

	^ paletteDictionary at: #caretColor ifAbsentPut: [ colorConfigurator caretColor ]
]

{ #category : 'colors' }
UIThemePalette >> classExtensionColor [

	^ paletteDictionary at: #classExtensionColor ifAbsentPut: [ colorConfigurator classExtensionColor ]
]

{ #category : 'accessing' }
UIThemePalette >> colorConfigurator: aColorConfigurator [

	colorConfigurator := aColorConfigurator
]

{ #category : 'colors' }
UIThemePalette >> dangerBackgroundColor [

	^ paletteDictionary at: #dangerBackgroundColor ifAbsentPut: [ colorConfigurator dangerBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> dangerTextColor [

	^ paletteDictionary at: #dangerTextColor ifAbsentPut: [ colorConfigurator dangerTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> darkBaseColor [

	^ paletteDictionary at: #darkBaseColor ifAbsentPut: [ colorConfigurator darkBaseColor ]
]

{ #category : 'colors' }
UIThemePalette >> desktopColor [

	^ paletteDictionary at: #desktopColor ifAbsentPut: [ colorConfigurator desktopColor ]
]

{ #category : 'colors' }
UIThemePalette >> diffAdditionalLineBackgroundColor [

	^ paletteDictionary at: #diffAdditionalLineBackgroundColor ifAbsentPut: [ colorConfigurator diffAdditionalLineBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> diffAdditionalTextInLineBackgroundColor [

	^ paletteDictionary at: #diffAdditionalTextInLineBackgroundColor ifAbsentPut: [ colorConfigurator diffAdditionalTextInLineBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> diffEdgeColor [

	^ paletteDictionary at: #diffEdgeColor ifAbsentPut: [ colorConfigurator diffEdgeColor ]
]

{ #category : 'colors' }
UIThemePalette >> diffJoinColor [

	^ paletteDictionary at: #diffJoinColor ifAbsentPut: [ colorConfigurator diffJoinColor ]
]

{ #category : 'colors' }
UIThemePalette >> diffMapColor [

	^ paletteDictionary at: #diffMapColor ifAbsentPut: [ colorConfigurator diffMapColor ]
]

{ #category : 'colors' }
UIThemePalette >> diffModificatedLineBackgroundColor [

	^ paletteDictionary at: #diffModificatedLineBackgroundColor ifAbsentPut: [ colorConfigurator diffModificatedLineBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> diffRemovalHighlightColor [

	^ paletteDictionary at: #diffRemovalHighlightColor ifAbsentPut: [ colorConfigurator diffRemovalHighlightColor ]
]

{ #category : 'colors' }
UIThemePalette >> diffRemovedLinesBackgroundColor [

	^ paletteDictionary at: #diffRemovedLinesBackgroundColor ifAbsentPut: [ colorConfigurator diffRemovedLinesBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> disabledColor [

	^ paletteDictionary at: #disabledColor ifAbsentPut: [ colorConfigurator disabledColor ]
]

{ #category : 'colors' }
UIThemePalette >> disabledLabelColor [

	^ paletteDictionary at: #disabledLabelColor ifAbsentPut: [ colorConfigurator disabledLabelColor ]
]

{ #category : 'colors' }
UIThemePalette >> disabledTextColor [

	^ paletteDictionary at: #disabledTextColor ifAbsentPut: [ colorConfigurator disabledTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> dropListNormalFillStyleFor: anObject [

	^ paletteDictionary at: #dropListNormalFillStyleFor: ifAbsentPut: [ colorConfigurator dropListNormalFillStyleFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> dropListNormalListFillStyleFor: anObject [

	^ paletteDictionary at: #dropListNormalListFillStyleFor: ifAbsentPut: [ colorConfigurator dropListNormalListFillStyleFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> enabledColor [

	^ paletteDictionary at: #enabledColor ifAbsentPut: [ colorConfigurator enabledColor ]
]

{ #category : 'colors' }
UIThemePalette >> errorPopperBackgroundColor [

	^ paletteDictionary at: #errorPopperBackgroundColor ifAbsentPut: [ colorConfigurator errorPopperBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> ghostTextColor [

	^ paletteDictionary at: #ghostTextColor ifAbsentPut: [ colorConfigurator ghostTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> growlBorderColorFor: aGrowlMorph [

	^ paletteDictionary at: #growlBorderColorFor: ifAbsentPut: [ colorConfigurator growlBorderColorFor: aGrowlMorph ]
]

{ #category : 'colors' }
UIThemePalette >> growlContentsColorFor: aGrowlMorph [

	^ paletteDictionary at: #growlContentsColorFor:
		  ifAbsentPut: [ colorConfigurator growlContentsColorFor: aGrowlMorph ]
]

{ #category : 'colors' }
UIThemePalette >> growlFillColorFor: aGrowlMorph [

	^ paletteDictionary at: #growlFillColorFor: ifAbsentPut: [ colorConfigurator growlFillColorFor: aGrowlMorph ]
]

{ #category : 'colors' }
UIThemePalette >> growlLabelColorFor: aGrowlMorph [

	^ paletteDictionary at: #growlLabelColorFor: ifAbsentPut: [ colorConfigurator growlLabelColorFor: aGrowlMorph ]
]

{ #category : 'basic-colors' }
UIThemePalette >> highlightTextColor [
	"I am a color used to have a text color that is more visible than the classic text color to do some highlights."

	^ paletteDictionary at: #highlightTextColor ifAbsentPut: [ colorConfigurator highlightTextColor ]
]

{ #category : 'initialization' }
UIThemePalette >> initialize [

	super initialize.
	paletteDictionary := Dictionary new
]

{ #category : 'colors' }
UIThemePalette >> lessConspicuousColorFrom: anObject [

	^ paletteDictionary at: #lessConspicuousColorFrom: ifAbsentPut: [ colorConfigurator lessConspicuousColorFrom: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> lightBackgroundColor [

	^ paletteDictionary at: #lightBackgroundColor ifAbsentPut: [ colorConfigurator lightBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> lightBaseColor [

	^ paletteDictionary at: #lightBaseColor ifAbsentPut: [ colorConfigurator lightBaseColor ]
]

{ #category : 'colors' }
UIThemePalette >> lightColorFor: anObject [

	^ paletteDictionary at: #lightColorFor: ifAbsentPut: [ colorConfigurator lightColorFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> lightSelectionColor [

	^ paletteDictionary at: #lightSelectionColor ifAbsentPut: [ colorConfigurator lightSelectionColor ]
]

{ #category : 'colors' }
UIThemePalette >> lineNumberColor [

	^ paletteDictionary at: #lineNumberColor ifAbsentPut: [ colorConfigurator lineNumberColor ]
]

{ #category : 'colors' }
UIThemePalette >> listBackgroundColor [

	^ paletteDictionary at: #listBackgroundColor ifAbsentPut: [ colorConfigurator listBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> listNormalFillStyleFor: anObject [

	^ paletteDictionary at: #listNormalFillStyleFor: ifAbsentPut: [ colorConfigurator listNormalFillStyleFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> listTextColor [

	^ paletteDictionary at: #listTextColor ifAbsentPut: [ colorConfigurator listTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> menuBorderColor [

	^ paletteDictionary at: #menuBorderColor ifAbsentPut: [ colorConfigurator menuBorderColor ]
]

{ #category : 'colors' }
UIThemePalette >> menuColor [

	^ paletteDictionary at: #menuColor ifAbsentPut: [ colorConfigurator menuColor ]
]

{ #category : 'colors' }
UIThemePalette >> menuItemDisabledTextColorFor: anObject [

	^ paletteDictionary at: #menuItemDisabledTextColorFor: ifAbsentPut: [ colorConfigurator menuItemDisabledTextColorFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> menuKeyboardFocusColor [

	^ paletteDictionary at: #menuKeyboardFocusColor ifAbsentPut: [ colorConfigurator menuKeyboardFocusColor ]
]

{ #category : 'colors' }
UIThemePalette >> menuSelectionColor [

	^ paletteDictionary at: #menuSelectionColor ifAbsentPut: [ colorConfigurator menuSelectionColor ]
]

{ #category : 'colors' }
UIThemePalette >> menuShadowColor [

	^ paletteDictionary at: #menuShadowColor ifAbsentPut: [ colorConfigurator menuShadowColor ]
]

{ #category : 'colors' }
UIThemePalette >> menuTitleColor [

	^ paletteDictionary at: #menuTitleColor ifAbsentPut: [ colorConfigurator menuTitleColor ]
]

{ #category : 'colors' }
UIThemePalette >> modalBackdropColor [

	^ paletteDictionary at: #modalBackdropColor ifAbsentPut: [ colorConfigurator modalBackdropColor ]
]

{ #category : 'colors' }
UIThemePalette >> mouseOverColor [

	^ paletteDictionary at: #mouseOverColor ifAbsentPut: [ colorConfigurator mouseOverColor ]
]

{ #category : 'colors' }
UIThemePalette >> paginatorSelectionColor [

	^ paletteDictionary at: #paginatorSelectionColor ifAbsentPut: [ colorConfigurator paginatorSelectionColor ]
]

{ #category : 'colors' }
UIThemePalette >> patchworkUIThemeColorFor: anObject [

	^ paletteDictionary at: #patchworkUIThemeColorFor: ifAbsentPut: [ colorConfigurator patchworkUIThemeColorFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> popoverButtonColor [

	^ paletteDictionary at: #popoverButtonColor ifAbsentPut: [ colorConfigurator popoverButtonColor ]
]

{ #category : 'colors' }
UIThemePalette >> progressBarColor [

	^ paletteDictionary at: #progressBarColor ifAbsentPut: [ colorConfigurator progressBarColor ]
]

{ #category : 'colors' }
UIThemePalette >> resizerGripNormalFillStyleFor: aResizer [

	^ paletteDictionary at: #resizerGripNormalFillStyleFor: ifAbsentPut: [ colorConfigurator resizerGripNormalFillStyleFor: aResizer ]
]

{ #category : 'colors' }
UIThemePalette >> scrollbarColor [

	^ paletteDictionary at: #scrollbarColor ifAbsentPut: [ colorConfigurator scrollbarColor ]
]

{ #category : 'colors' }
UIThemePalette >> scrollbarColorFor: aScrollbar [

	^ paletteDictionary at: #scrollbarColorFor: ifAbsentPut: [ colorConfigurator scrollbarColorFor: aScrollbar ]
]

{ #category : 'colors' }
UIThemePalette >> scrollbarImageColorFor: aScrollbar [

	^ paletteDictionary at: #scrollbarImageColorFor: ifAbsentPut: [ colorConfigurator scrollbarImageColorFor: aScrollbar ]
]

{ #category : 'colors' }
UIThemePalette >> scrollbarNormalFillStyleFor: aScrollbar [

	^ paletteDictionary at: #scrollbarNormalFillStyleFor: ifAbsentPut: [ colorConfigurator scrollbarNormalFillStyleFor: aScrollbar ]
]

{ #category : 'colors' }
UIThemePalette >> secondarySelectionColor [

	^ paletteDictionary at: #secondarySelectionColor ifAbsentPut: [ colorConfigurator secondarySelectionColor ]
]

{ #category : 'colors' }
UIThemePalette >> selectedItemListTextColor [

	^ paletteDictionary at: #selectedItemListTextColor ifAbsentPut: [ colorConfigurator selectedItemListTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> selectionBarColor [

	^ paletteDictionary at: #selectionBarColor ifAbsentPut: [ colorConfigurator selectionBarColor ]
]

{ #category : 'colors' }
UIThemePalette >> selectionColor [

	^ paletteDictionary at: #selectionColor ifAbsentPut: [ colorConfigurator selectionColor ]
]

{ #category : 'colors' }
UIThemePalette >> selectionTextColor [

	^ paletteDictionary at: #selectionTextColor ifAbsentPut: [ colorConfigurator selectionTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> sliderNormalFillStyleFor: anObject [

	^ paletteDictionary at: #sliderNormalFillStyleFor: ifAbsentPut: [ colorConfigurator sliderNormalFillStyleFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> spotlightWindowColor [

	^ paletteDictionary at: #spotlightWindowColor ifAbsentPut: [ colorConfigurator spotlightWindowColor ]
]

{ #category : 'colors' }
UIThemePalette >> subgroupColorFrom: anObject [

	^ paletteDictionary at: #subgroupColorFrom: ifAbsentPut: [ colorConfigurator subgroupColorFrom: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> successBackgroundColor [

	^ paletteDictionary at: #successBackgroundColor ifAbsentPut: [ colorConfigurator successBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> successTextColor [

	^ paletteDictionary at: #successTextColor ifAbsentPut: [ colorConfigurator successTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> taskbarActiveButtonColorFor: anObject [

	^ paletteDictionary at: #taskbarActiveButtonColorFor: ifAbsentPut: [ colorConfigurator taskbarActiveButtonColorFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> taskbarButtonColorFor: anObject [

	^ paletteDictionary at: #taskbarButtonColorFor: ifAbsentPut: [ colorConfigurator taskbarButtonColorFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> taskbarButtonLabelColorFor: aButton [

	^ paletteDictionary at: #taskbarButtonLabelColorFor: ifAbsentPut: [ colorConfigurator taskbarButtonLabelColorFor: aButton ]
]

{ #category : 'colors' }
UIThemePalette >> taskbarItemLabelColorForCollapsed: anObject [

	^ paletteDictionary at: #taskbarItemLabelColorForCollapsed: ifAbsentPut: [ colorConfigurator taskbarItemLabelColorForCollapsed: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> taskbarItemLabelColorForExpanded: anObject [

	^ paletteDictionary at: #taskbarItemLabelColorForExpanded: ifAbsentPut: [ colorConfigurator taskbarItemLabelColorForExpanded: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> taskbarMinimizedButtonColorFor: anObject [

	^ paletteDictionary at: #taskbarMinimizedButtonColorFor: ifAbsentPut: [ colorConfigurator taskbarMinimizedButtonColorFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> textColor [

	^ paletteDictionary at: #textColor ifAbsentPut: [ colorConfigurator textColor ]
]

{ #category : 'colors' }
UIThemePalette >> textColorForNonStandardBackground [

	^ paletteDictionary at: #textColorForNonStandardBackground ifAbsentPut: [ colorConfigurator textColorForNonStandardBackground ]
]

{ #category : 'colors' }
UIThemePalette >> textEditorNormalFillStyleFor: anObject [

	^ paletteDictionary at: #textEditorNormalFillStyleFor: ifAbsentPut: [ colorConfigurator textEditorNormalFillStyleFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> textFieldNormalFillStyleFor: aTextField [

	^ paletteDictionary at: #textFieldNormalFillStyleFor: ifAbsentPut: [ colorConfigurator textFieldNormalFillStyleFor: aTextField ]
]

{ #category : 'colors' }
UIThemePalette >> themeSettings [

	^ paletteDictionary at: #themeSettings ifAbsentPut: [ colorConfigurator themeSettings ]
]

{ #category : 'colors' }
UIThemePalette >> themeSettings: anObject [

	^ paletteDictionary at: #themeSettings: ifAbsentPut: [ colorConfigurator themeSettings: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> treeLineColorsFrom: aColor [

	^ paletteDictionary at: #treeLineColorsFrom: ifAbsentPut: [ colorConfigurator treeLineColorsFrom: aColor ]
]

{ #category : 'colors' }
UIThemePalette >> unfocusedSelectionColor [

	^ paletteDictionary at: #unfocusedSelectionColor ifAbsentPut: [ colorConfigurator unfocusedSelectionColor ]
]

{ #category : 'colors' }
UIThemePalette >> urlColor [

	^ paletteDictionary at: #urlColor ifAbsentPut: [ colorConfigurator urlColor ]
]

{ #category : 'colors' }
UIThemePalette >> veryLightSelectionColor [

	^ paletteDictionary at: #veryLightSelectionColor ifAbsentPut: [ colorConfigurator veryLightSelectionColor ]
]

{ #category : 'colors' }
UIThemePalette >> warningBackgroundColor [

	^ paletteDictionary at: #warningBackgroundColor ifAbsentPut: [ colorConfigurator warningBackgroundColor ]
]

{ #category : 'colors' }
UIThemePalette >> warningTextColor [

	^ paletteDictionary at: #warningTextColor ifAbsentPut: [ colorConfigurator warningTextColor ]
]

{ #category : 'colors' }
UIThemePalette >> windowActiveLabelFillStyleFor: anObject [

	^ paletteDictionary at: #windowActiveLabelFillStyleFor: ifAbsentPut: [ colorConfigurator windowActiveLabelFillStyleFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> windowColor [

	^ paletteDictionary at: #windowColor ifAbsentPut: [ colorConfigurator windowColor ]
]

{ #category : 'colors' }
UIThemePalette >> windowColorFor: anObject [

	^ paletteDictionary at: #windowColorFor: ifAbsentPut: [ colorConfigurator windowColorFor: anObject  ]
]

{ #category : 'colors' }
UIThemePalette >> windowEdgeNormalFillStyleFor: anEdgeGrip [

	^ paletteDictionary at: #windowEdgeNormalFillStyleFor: ifAbsentPut: [ colorConfigurator windowEdgeNormalFillStyleFor: anEdgeGrip ]
]

{ #category : 'colors' }
UIThemePalette >> windowShadowColor [

	^ paletteDictionary at: #windowShadowColor ifAbsentPut: [ colorConfigurator windowShadowColor ]
]

{ #category : 'colors' }
UIThemePalette >> worldMainDockingBarColorFor: anObject [

	^ paletteDictionary at: #worldMainDockingBarColorFor: ifAbsentPut: [ colorConfigurator worldMainDockingBarColorFor: anObject  ]
]

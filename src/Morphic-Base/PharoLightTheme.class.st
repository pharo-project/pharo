"
The theme is developed in the context of the Glamour project, and its goal is to create a look that:

- does not look like a specific operating system. In particular, the icons should be operating system agnostic, because, for example, people in Windows are confused by the red, yellow, green buttons of apple.
- uses a limited amount of colors and effects.
- is fast.
"
Class {
	#name : 'PharoLightTheme',
	#superclass : 'UITheme',
	#category : 'Morphic-Base-Theme',
	#package : 'Morphic-Base',
	#tag : 'Theme'
}

{ #category : 'configuration' }
PharoLightTheme class >> colorConfiguratorClass [

	^ LightThemeColorConfigurator
]

{ #category : 'testing' }
PharoLightTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^false
]

{ #category : 'accessing' }
PharoLightTheme class >> themeName [
	^ 'Pharo Light'
]

{ #category : 'border-styles - buttons' }
PharoLightTheme >> buttonDisabledBorderStyleFor: aButton [
	"Return the disabled button borderStyle for the given button."

	^ BorderStyle width: 0 color: Color transparent
]

{ #category : 'fill-styles - buttons' }
PharoLightTheme >> buttonMouseOverFillStyleFor: aButton [
	"Return the button mouse over fillStyle for the given color."

	(aButton valueOfProperty: #noFill ifAbsent: [ false ]) ifTrue: [
		^ self buttonNormalFillStyleFor: aButton ].
	^ SolidFillStyle color: self mouseOverColor
]

{ #category : 'border-styles - buttons' }
PharoLightTheme >> buttonNormalBorderStyleFor: aButton [
	"Return the normal button borderStyle for the given button."

	| outerColor |
	(aButton valueOfProperty: #noBorder ifAbsent: [false])
		ifTrue: [ ^ SimpleBorderStyle new
							width: 0;
							baseColor: Color transparent ].
	outerColor := self darkBaseColorFor: aButton.
	^ SimpleBorderStyle new
			width: 1 scaledByDisplayScaleFactor;
			baseColor: outerColor
]

{ #category : 'border-styles' }
PharoLightTheme >> configureWindowBorderFor: aWindow [

	aWindow borderStyle: (SimpleBorderStyle new
			 color: self borderColor;
			 width: 1 scaledByDisplayScaleFactor)
]

{ #category : 'initialization' }
PharoLightTheme >> defaultSettings [

	^ super defaultSettings
		  menuColor: self menuColor;
		  menuTitleColor: self menuTitleColor;
		  menuSelectionTextColor: Color white;
		  progressBarColor: self lightBackgroundColor;
		  flatMenu: true;
		  yourself
]

{ #category : 'initialization' }
PharoLightTheme >> initialize [
	"self beCurrent"

	super initialize.
	self windowActiveDropShadowStyle: #diffuse.
	settings preferGradientFill: false
]

{ #category : 'accessing' }
PharoLightTheme >> menuBorderWidth [

	^ 1 scaledByDisplayScaleFactor
]

{ #category : 'defaults' }
PharoLightTheme >> menuInset [

	^ 4 scaledByDisplayScaleFactor
]

{ #category : 'defaults' }
PharoLightTheme >> menuItemVerticalPadding [

	^ 5 scaledByDisplayScaleFactor
]

{ #category : 'fill-styles - scrollbars' }
PharoLightTheme >> scrollbarMouseOverThumbFillStyleFor: aScrollbar [
	"Return the scrollbar mouse over thumb fillStyle for the given scrollbar."

	^ SolidFillStyle color: self disabledColor
]

{ #category : 'fill-styles - scrollbars' }
PharoLightTheme >> scrollbarNormalButtonFillStyleFor: aScrollbar [
	"Keep the arrow buttons but draw them as part of the track."

	^ self scrollbarNormalFillStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
PharoLightTheme >> scrollbarNormalThumbBorderStyleFor: aScrollbar [
	"A border in the color of the track makes the thumb look inset without any extra drawing."

	^ SimpleBorderStyle new
		  width: 2 scaledByDisplayScaleFactor;
		  baseColor: (self scrollbarNormalFillStyleFor: aScrollbar) asColor
]

{ #category : 'fill-styles - scrollbars' }
PharoLightTheme >> scrollbarNormalThumbFillStyleFor: aScrollbar [
	"Return the normal scrollbar thumb fillStyle for the given scrollbar."

	^ SolidFillStyle color: self scrollbarColor
]

{ #category : 'fill-styles - scrollbars' }
PharoLightTheme >> scrollbarPressedThumbFillStyleFor: aScrollbar [
	"Return the pressed scrollbar thumb fillStyle for the given scrollbar."
	"Return the normal scrollbar fillStyle for the given scrollbar."
	"^ SolidFillStyle color: Color lightGray lighter."

	^ self selectedFillStyleFor: aScrollbar height: aScrollbar height
]

{ #category : 'defaults' }
PharoLightTheme >> scrollbarThickness [
	"Answer the width or height of a scrollbar as appropriate to
	its orientation."

	^ 11 * self displayScaleFactor
]

{ #category : 'border-styles' }
PharoLightTheme >> tabLabelNormalBorderStyleFor: aTabLabel [

	^ BottomBorderStyle new
		  color: Color transparent;
		  width: 2 scaledByDisplayScaleFactor;
		  yourself
]

{ #category : 'fill-styles - buttons' }
PharoLightTheme >> tabLabelNormalFillStyleFor: aTabLabel [

	^ SolidFillStyle color: Color transparent
]

{ #category : 'border-styles' }
PharoLightTheme >> tabLabelSelectedBorderStyleFor: aTabLabel [

	^ self tabLabelNormalBorderStyleFor: aTabLabel
]

{ #category : 'fill-styles - buttons' }
PharoLightTheme >> tabLabelSelectedFillStyleFor: aTabLabel [

	^ SolidFillStyle color: self backgroundColor
]

{ #category : 'accessing - colors' }
PharoLightTheme >> tabSelectionIndicatorColor [

	^ self selectedTabBorderColor
]

{ #category : 'fill-styles - taskbar' }
PharoLightTheme >> taskbarFillStyleFor: aTaskbar [

	^ SolidFillStyle color: self baseColor
]

{ #category : 'border-styles - taskbar' }
PharoLightTheme >> taskbarItemDisabledBorderStyleFor: aButton [
	^ self taskbarItemNormalBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemDisabledFillStyleFor: aButton [
	"Return the disabled taskbar item fillStyle for the given color."

	^ self taskbarItemNormalFillStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
PharoLightTheme >> taskbarItemMouseOverBorderStyleFor: aButton [

	^ BottomBorderStyle new
		  color: self selectedTabBorderColor;
		  width: 2 scaledByDisplayScaleFactor;
		  yourself
]

{ #category : 'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemMouseOverFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.07)
]

{ #category : 'border-styles - taskbar' }
PharoLightTheme >> taskbarItemNormalBorderStyleFor: aButton [

	^ BottomBorderStyle new
		  color: self borderColor;
		  width: 2 scaledByDisplayScaleFactor;
		  trimSize: 2 scaledByDisplayScaleFactor;
		  yourself
]

{ #category : 'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemNormalFillStyleFor: aButton [
	^ SolidFillStyle color: Color transparent
]

{ #category : 'border-styles - taskbar' }
PharoLightTheme >> taskbarItemPressedBorderStyleFor: aButton [
	^ self taskbarItemNormalBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemPressedFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.15)
]

{ #category : 'border-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedBorderStyleFor: aButton [
	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedDisabledBorderStyleFor: aButton [
	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.1)
]

{ #category : 'border-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedMouseOverBorderStyleFor: aButton [
	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedMouseOverFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.15)
]

{ #category : 'border-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedPressedBorderStyleFor: aButton [
	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoLightTheme >> taskbarItemSelectedPressedFillStyleFor: aButton [
	^ SolidFillStyle color: (Color black alpha: 0.2)
]

{ #category : 'fill-styles' }
PharoLightTheme >> windowActiveTitleFillStyleFor: aWindow [

	^ SolidFillStyle color: self lightBackgroundColor
]

{ #category : 'fill-styles' }
PharoLightTheme >> windowInactiveLabelFillStyleFor: aWindow [

	^ self disabledColor
]

{ #category : 'fill-styles' }
PharoLightTheme >> worldMainDockingBarNormalFillStyleFor: aDockingBar [
	"A solid fill keeps the bar cheap to redraw."

	^ SolidFillStyle color: aDockingBar originalColor
]

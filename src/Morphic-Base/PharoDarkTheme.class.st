"
A dark theme for pharo, inspired on 

http://blog.jetbrains.com/blog/2013/06/11/inside-darcula-look-and-feel-an-interview-with-konstantin-bulenkov/

This theme works well with the ""Dark"" text styling.
"
Class {
	#name : 'PharoDarkTheme',
	#superclass : 'UITheme',
	#category : 'Morphic-Base-Theme',
	#package : 'Morphic-Base',
	#tag : 'Theme'
}

{ #category : 'configuration' }
PharoDarkTheme class >> colorConfiguratorClass [

	^ DarkThemeColorConfigurator
]

{ #category : 'testing' }
PharoDarkTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^false
]

{ #category : 'accessing' }
PharoDarkTheme class >> themeName [
	^ 'Pharo Dark'
]

{ #category : 'colors' }
PharoDarkTheme >> badgeColor [
	^ Color red alpha: 0.5
]

{ #category : 'colors' }
PharoDarkTheme >> badgeTextColor [
	^ self textColor
]

{ #category : 'border-styles - buttons' }
PharoDarkTheme >> buttonDisabledBorderStyleFor: aButton [

	"Return the disabled button fillStyle for the given color."

	^ BorderStyle width: 0 color: Color transparent
]

{ #category : 'fill-styles - buttons' }
PharoDarkTheme >> buttonMouseOverFillStyleFor: aButton [
	"Return the button mouse over fillStyle for the given color."

	(aButton valueOfProperty: #noFill ifAbsent: [ false ]) ifTrue: [
		^ self buttonNormalFillStyleFor: aButton ].
	^ SolidFillStyle color: self buttonColor lighter
]

{ #category : 'border-styles - buttons' }
PharoDarkTheme >> buttonNormalBorderStyleFor: aButton [

	"Return the normal button borderStyle for the given button."

	(aButton valueOfProperty: #noBorder ifAbsent: [ false ]) ifTrue: [
		^ SimpleBorderStyle new
			  width: 0;
			  baseColor: Color transparent ].
	^ SimpleBorderStyle new
		  width: 1 scaledByDisplayScaleFactor;
		  baseColor: self buttonColor
]

{ #category : 'fill-styles - buttons' }
PharoDarkTheme >> buttonNormalFillStyleFor: aButton [

	"Return the normal button fillStyle for the given button."

	(aButton valueOfProperty: #noFill ifAbsent: [ false ]) ifTrue: [
		^ SolidFillStyle color: Color transparent ].
	^ SolidFillStyle color: self buttonColor
]

{ #category : 'fill-styles - buttons' }
PharoDarkTheme >> buttonSelectedFillStyleFor: aButton [

	"Return the normal button fillStyle for the given button."


	^ SolidFillStyle color: self selectionColor
]

{ #category : 'border-styles' }
PharoDarkTheme >> configureWindowBorderFor: aWindow [

	aWindow borderStyle: (SimpleBorderStyle new
			 color: self borderColor;
			 width: 1 scaledByDisplayScaleFactor)
]

{ #category : 'fill-styles - buttons' }
PharoDarkTheme >> controlButtonNormalFillStyleFor: aButton [

	"Return the normal control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^ self buttonNormalFillStyleFor: aButton
]

{ #category : 'initialization' }
PharoDarkTheme >> defaultSettings [

	^ super defaultSettings
		  menuColor: self menuColor;
		  menuTitleColor: self menuTitleColor;
		  menuSelectionTextColor: Color white;
		  progressBarColor: self lightBackgroundColor;
		  flatMenu: true;
		  yourself
]

{ #category : 'border-styles' }
PharoDarkTheme >> dropListNormalListBorderStyleFor: aDropList [

	"Return the normal borderStyle for the list of the given given drop list"

	^ BorderStyle inset
		  width: 1 scaledByDisplayScaleFactor;
		  baseColor: self buttonColor
]

{ #category : 'initialization' }
PharoDarkTheme >> initialize [

	super initialize.
	self windowActiveDropShadowStyle: #diffuse.
	self settings preferGradientFill: false
]

{ #category : 'accessing' }
PharoDarkTheme >> menuBorderWidth [

	^ 1 scaledByDisplayScaleFactor
]

{ #category : 'defaults' }
PharoDarkTheme >> menuInset [

	^ 4 scaledByDisplayScaleFactor
]

{ #category : 'defaults' }
PharoDarkTheme >> menuItemVerticalPadding [

	^ 5 scaledByDisplayScaleFactor
]

{ #category : 'accessing - colors' }
PharoDarkTheme >> popoverButtonColor [

	^ self lightBaseColor
]

{ #category : 'fill-styles - scrollbars' }
PharoDarkTheme >> scrollbarMouseOverThumbFillStyleFor: aScrollbar [
	"Return the scrollbar mouse over thumb fillStyle for the given scrollbar."

	^ SolidFillStyle color: self disabledColor
]

{ #category : 'fill-styles - scrollbars' }
PharoDarkTheme >> scrollbarNormalButtonFillStyleFor: aScrollbar [
	"Keep the arrow buttons but draw them as part of the track."

	^ self scrollbarNormalFillStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
PharoDarkTheme >> scrollbarNormalThumbBorderStyleFor: aScrollbar [
	"A border in the color of the track makes the thumb look inset without any extra drawing."

	^ SimpleBorderStyle new
		  width: 2 scaledByDisplayScaleFactor;
		  baseColor: (self scrollbarNormalFillStyleFor: aScrollbar) asColor
]

{ #category : 'fill-styles - scrollbars' }
PharoDarkTheme >> scrollbarNormalThumbFillStyleFor: aScrollbar [
	"Return the normal scrollbar thumb fillStyle for the given scrollbar."

	^ SolidFillStyle color: self scrollbarColor
]

{ #category : 'fill-styles - scrollbars' }
PharoDarkTheme >> scrollbarPressedThumbFillStyleFor: aScrollbar [

	"Return the normal scrollbar button fillStyle for the given scrollbar."

	^ self buttonSelectedFillStyleFor: aScrollbar
]

{ #category : 'defaults' }
PharoDarkTheme >> scrollbarThickness [
	"Answer the width or height of a scrollbar as appropriate to
	its orientation."

	^ 11 * self displayScaleFactor
]

{ #category : 'accessing' }
PharoDarkTheme >> shStyleTableName [

	^ #darkStyleTable
]

{ #category : 'border-styles' }
PharoDarkTheme >> tabLabelNormalBorderStyleFor: aTabLabel [

	^ BottomBorderStyle new
		  color: Color transparent;
		  width: 2 scaledByDisplayScaleFactor;
		  yourself
]

{ #category : 'fill-styles - buttons' }
PharoDarkTheme >> tabLabelNormalFillStyleFor: aTabLabel [

	^ SolidFillStyle color: Color transparent
]

{ #category : 'border-styles' }
PharoDarkTheme >> tabLabelSelectedBorderStyleFor: aTabLabel [

	^ self tabLabelNormalBorderStyleFor: aTabLabel
]

{ #category : 'fill-styles - buttons' }
PharoDarkTheme >> tabLabelSelectedFillStyleFor: aTabLabel [

	^ SolidFillStyle color: self backgroundColor
]

{ #category : 'accessing - colors' }
PharoDarkTheme >> tabSelectionIndicatorColor [

	^ self selectedTabBorderColor
]

{ #category : 'fill-styles - taskbar' }
PharoDarkTheme >> taskbarFillStyleFor: aTaskbar [

	^ SolidFillStyle color: self baseColor
]

{ #category : 'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemDisabledBorderStyleFor: aButton [

	^ self taskbarItemNormalBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemDisabledFillStyleFor: aButton [

	"Return the disabled taskbar item fillStyle for the given color."

	^ self taskbarItemNormalFillStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemMouseOverBorderStyleFor: aButton [

	^ BottomBorderStyle new
		  color: self selectedTabBorderColor;
		  width: 2 scaledByDisplayScaleFactor;
		  yourself
]

{ #category : 'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemMouseOverFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.15)
]

{ #category : 'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemNormalBorderStyleFor: aButton [

	^ BottomBorderStyle new
		  color: self borderColor;
		  width: 2 scaledByDisplayScaleFactor;
		  trimSize: 2 scaledByDisplayScaleFactor;
		  yourself
]

{ #category : 'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemNormalFillStyleFor: aButton [

	^ SolidFillStyle color: Color transparent
]

{ #category : 'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemPressedBorderStyleFor: aButton [

	^ self taskbarItemNormalBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemPressedFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.4)
]

{ #category : 'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedBorderStyleFor: aButton [

	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedDisabledBorderStyleFor: aButton [

	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.15)
]

{ #category : 'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedMouseOverBorderStyleFor: aButton [

	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedMouseOverFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.25)
]

{ #category : 'border-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedPressedBorderStyleFor: aButton [

	^ self taskbarItemMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
PharoDarkTheme >> taskbarItemSelectedPressedFillStyleFor: aButton [

	^ SolidFillStyle color: (Color white alpha: 0.35)
]

{ #category : 'fill-styles' }
PharoDarkTheme >> windowActiveTitleFillStyleFor: aWindow [

	^ SolidFillStyle color: self lightBackgroundColor
]

{ #category : 'label-styles - windows' }
PharoDarkTheme >> windowCloseFormSet [
	"Answer the form set to use for the close button of a window."

	^ self iconFormSetNamed: #windowCloseInverted
]

{ #category : 'label-styles - windows' }
PharoDarkTheme >> windowCloseOverFormSet [
	"Answer the form set to use for mouse over window close buttons"

	^ self iconFormSetNamed: #windowCloseInverted
]

{ #category : 'fill-styles' }
PharoDarkTheme >> windowInactiveLabelFillStyleFor: aWindow [

	^ self disabledColor
]

{ #category : 'label-styles - windows' }
PharoDarkTheme >> windowLabelForText: aTextOrString [

	^ (super windowLabelForText: aTextOrString)
		  color: self textColor;
		  yourself
]

{ #category : 'label-styles - windows' }
PharoDarkTheme >> windowMaximizeFormSet [
	"Answer the form set to use for the maximize button of a window."

	^ self iconFormSetNamed: #windowMaximizeInverted
]

{ #category : 'label-styles - windows' }
PharoDarkTheme >> windowMaximizeOverFormSet [
	"Answer the form set to use for mouse over window maximize buttons"

	^ self iconFormSetNamed: #windowMaximizeInverted
]

{ #category : 'label-styles - windows' }
PharoDarkTheme >> windowMenuFormSet [
	"Answer the form set to use for the menu button of a window."

	^ self iconFormSetNamed: #windowMenuInverted
]

{ #category : 'label-styles - windows' }
PharoDarkTheme >> windowMinimizeFormSet [
	"Answer the form set to use for the minimize button of a window."

	^ self iconFormSetNamed: #windowMinimizeInverted
]

{ #category : 'label-styles - windows' }
PharoDarkTheme >> windowMinimizeOverFormSet [
	"Answer the form set to use for mouse over window minimize buttons"

	^ self iconFormSetNamed: #windowMinimizeInverted
]

{ #category : 'fill-styles' }
PharoDarkTheme >> worldMainDockingBarNormalFillStyleFor: aDockingBar [
	"A solid fill keeps the bar cheap to redraw."

	^ SolidFillStyle color: aDockingBar originalColor
]

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

	| aStyle |
	aStyle := SimpleBorderStyle new
					color: (Color lightGray);
					width: 1 scaledByDisplayScaleFactor.
	aWindow borderStyle: aStyle
]

{ #category : 'initialization' }
PharoLightTheme >> initialize [
	"self beCurrent"

	super initialize.
	self windowActiveDropShadowStyle: #diffuse.
	settings preferGradientFill: false
]

{ #category : 'fill-styles - scrollbars' }
PharoLightTheme >> scrollbarPressedThumbFillStyleFor: aScrollbar [
	"Return the pressed scrollbar thumb fillStyle for the given scrollbar."
	"Return the normal scrollbar fillStyle for the given scrollbar."
	"^ SolidFillStyle color: Color lightGray lighter."

	^ self selectedFillStyleFor: aScrollbar height: aScrollbar height
]

{ #category : 'fill-styles - taskbar' }
PharoLightTheme >> taskbarFillStyleFor: aTaskbar [
	^ SolidFillStyle color: (Color gray muchLighter alpha: 0.8)
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
		color: (Color fromHexString: '616161');
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
		color: (Color fromHexString: '757575');
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

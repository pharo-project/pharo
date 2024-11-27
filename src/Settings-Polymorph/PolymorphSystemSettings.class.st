"
I provide settings for Morphic appearance (theme, fonts, colors, ...) that can be found and changed in the Settings browser.
"
Class {
	#name : 'PolymorphSystemSettings',
	#superclass : 'Object',
	#classVars : [
		'DesktopColor',
		'DesktopGradientDirection',
		'DesktopGradientFillColor',
		'DesktopGradientOrigin',
		'DesktopImageFileName',
		'DesktopImageLayout',
		'DesktopLogo',
		'DesktopLogoFileName',
		'ShowDesktopLogo',
		'UseDesktopGradientFill'
	],
	#category : 'Settings-Polymorph',
	#package : 'Settings-Polymorph'
}

{ #category : 'settings' }
PolymorphSystemSettings class >> appearanceSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #appearance)
		label: 'Appearance';
		description: 'All settings concerned with the look''n feel of your system';
		with: [
			(aBuilder pickOne: #uiThemeClass)
				label: 'User interface theme';
				target: self;
				default: self defaultTheme;
				order: 1;
				domainValues: self uiThemeClassChoices.
			self morphicSettingsOn: aBuilder.
			self desktopSettingsOn: aBuilder ]
]

{ #category : 'settings' }
PolymorphSystemSettings class >> defaultTheme [

	^ DarkBlueTheme
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopBackgroundChanged [

	| fill |
	self desktopImageFileName
		ifEmpty: [ self currentWorld world backgroundMorph: nil ]
		ifNotEmpty: [
			[
			self currentWorld
				backgroundImage: (Form fromFileNamed: self desktopImageFileName)
				layout: self desktopImageLayout ]
				on: Error
				do: [  ] ].

	self useDesktopGradientFill
		ifTrue: [
			self currentWorld fillStyle isGradientFill
				ifTrue: [ fill := self currentWorld fillStyle ]
				ifFalse: [ fill := GradientFillStyle new ].
			fill colorRamp: {
					(0.0 -> self desktopColor darker).
					(1.0 -> self desktopGradientFillColor lighter) }.
			fill origin:
				(self currentWorld perform: self desktopGradientOrigin).
			self desktopGradientDirection = #Vertical ifTrue: [
				fill radial: false.
				fill normal: self currentWorld width @ 0.
				fill direction: 0 @ self currentWorld height ].
			self desktopGradientDirection = #Horizontal ifTrue: [
				fill radial: false.
				fill normal: 0 @ self currentWorld height.
				fill direction: self currentWorld width @ 0 ].
			self desktopGradientDirection = #Radial ifTrue: [
				fill radial: true.
				self desktopGradientOrigin = #center
					ifTrue: [
						fill direction: self currentWorld width // 2 @ 0.
						fill normal: 0 @ (self currentWorld height // 2) ]
					ifFalse: [
						fill direction: self currentWorld width @ 0.
						fill normal: 0 @ self currentWorld height ] ].
			self currentWorld fillStyle: fill ]
		ifFalse: [ self currentWorld color: self desktopColor ].

	self currentWorld changed
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopColor [
	^ DesktopColor ifNil: [DesktopColor := self currentWorld defaultWorldColor]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopColor: aColor [
	DesktopColor := aColor.
	self desktopBackgroundChanged
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopColorSettingsOn: aBuilder [
	(aBuilder setting: #desktopColor) label: 'Color';
		 description: 'This is the only color of your desktop if no gradient is used. If you make use of a gradient, then this color is the first one of the gradient';
		 default: PharoDarkTheme current desktopColor;
		with: [(aBuilder setting: #useDesktopGradientFill) label: 'Gradient';
				description: 'If true, then more settings will be available in order to define the desktop background color gradient';
				noOrdering;
				default: false;
				with: [(aBuilder setting: #desktopGradientFillColor) label: 'Other color';
						default: PharoDarkTheme current desktopColor;
						description: 'This is the second color of your gradient (the first one is given by the "Color" setting'.
					(aBuilder pickOne: #desktopGradientDirection) label: 'Direction';
						default: #Vertical;
						domainValues: {#Horizontal. #Vertical. #Radial}.
					(aBuilder pickOne: #desktopGradientOrigin) label: 'Origin';
						domainValues: {
							'Top left' translated -> #topLeft.
							'Top' translated -> #topCenter.
							'Top right' translated -> #topRight.
							'Right' translated -> #rightCenter.
							'Bottom right' translated -> #bottomRight.
							'Bottom' translated -> #bottomCenter.
							'Bottom left' translated -> #bottomLeft.
							'Left' translated -> #leftCenter.
							'Center' translated -> #center}]]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopGradientDirection [
	"#Radial #Vertical or #Horizontal"
	^ DesktopGradientDirection ifNil: [DesktopGradientDirection := #Vertical]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopGradientDirection: aSymbol [
	"#Radial #Vertical or #Horizontal"
	DesktopGradientDirection := aSymbol.
	self desktopBackgroundChanged
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopGradientFillColor [
	^ DesktopGradientFillColor ifNil: [DesktopGradientFillColor := self desktopColor]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopGradientFillColor: aColor [
	DesktopGradientFillColor := aColor.
	self desktopBackgroundChanged
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopGradientOrigin [
	"#topLeft #topCenter #topRight #rightCenter #bottomRight #bottomCenter #bottomLeft #leftCenter #center"
	^ DesktopGradientOrigin ifNil: [DesktopGradientOrigin := #topLeft]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopGradientOrigin: aSymbol [
	"#Radial #Vertical or #Horizontal"
	DesktopGradientOrigin := aSymbol.
	self desktopBackgroundChanged
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopImageFileName [
	^ DesktopImageFileName ifNil: [DesktopImageFileName := '']
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopImageFileName: aFileName [
	DesktopImageFileName := aFileName.
	self desktopBackgroundChanged
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopImageLayout [
	^ DesktopImageLayout ifNil: [DesktopImageLayout := #scaled]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopImageLayout: aSymbol [
	DesktopImageLayout := aSymbol.
	self desktopBackgroundChanged
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopLogo [
	^ DesktopLogo ifNil: [self showDesktopLogo ifTrue: [DesktopLogo := self pharoLogo]]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopLogo: anImageMorph [
	DesktopLogo := anImageMorph
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopLogoChanged [
	self showDesktopLogo
		ifFalse: [self setDesktopLogoWith: nil.
			^ self].
	self desktopLogoFileName
		ifEmpty: [self setDesktopLogoWith: self pharoLogo]
		ifNotEmpty: [[self
				setDesktopLogoWith: (ImageMorph withForm: (Form fromFileNamed: self desktopLogoFileName))]
				on: Error
				do: [self setDesktopLogoWith: self pharoLogo]]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopLogoFileName [
	^ DesktopLogoFileName ifNil: [DesktopLogoFileName := '']
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopLogoFileName: aFileName [
	DesktopLogoFileName = aFileName ifTrue: [^ self].
	DesktopLogoFileName := aFileName.
	self desktopLogoChanged
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> desktopSettingsOn: aBuilder [
	(aBuilder group: #desktopSettings)
		label: 'Desktop';
		target: self;
		noOrdering;
		with: [
			(aBuilder setting: #showDesktopLogo)
				label: 'Show logo' ;
				description: 'If true, then a logo if shown on the top left of the desktop' ;
				default: true;
				with: [
					(aBuilder setting: #desktopLogoFileName)
						type: #FilePathEncoder;
						description: 'The path of an image file for your own logo, the default pharo logo is used if empty.' ;
						label: 'Logo image file name'].
			self desktopColorSettingsOn: aBuilder.
			(aBuilder setting: #desktopImageFileName)
				type: #FilePathEncoder;
				default: '';
				description: 'The path of an image file which will be imported as a form and displayed as your desktop background';
				label: 'Desktop background image file name';
			with: [
				(aBuilder pickOne: #desktopImageLayout) label: 'Layout';
				 description: 'Size and placement of the background image';
				 target: self;
				 order: 0;
				 default: #scaled;
				 domainValues: #(#center #tiled #scaled #scaledAspect
		#topLeft #topCenter #topRight #rightCenter
		#bottomRight #bottomCenter #bottomLeft #leftCenter).
				]]
]

{ #category : 'morphic' }
PolymorphSystemSettings class >> labelAreaLayoutSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder pickOne: #labelAreaLayout)
		label: 'Label area layout' translated;
		parent: #windows;
		target: UITheme;
		targetSelector: #currentSettings;
		default: #platformDependent;
		domainValues: {
			'Platform Dependent' translated -> #platformDependent.
			'macOS' -> #macOS.
			'Windows/Linux' -> #WindowsOrLinux };
		description: 'If Platform Dependent, label area elements are placed as they are in the windows of the current OS.
If macOS, close/collapse/expand boxes are placed on the left, and menu control on the right.
If Windows/Linux, elements are placed in order: menu control, label, collapse/expand/close boxes.'
]

{ #category : 'settings' }
PolymorphSystemSettings class >> morphicHaloSettingsOn: aBuilder [
	(aBuilder group: #halo)
		label: 'Halo';
		parent: #morphic;
		description: 'All halo settings';
		noOrdering;
		with: [
			(aBuilder setting: #cycleHalosBothDirections)
				label: 'Cycle both directions';
				target: Morph;
				default: false;
				description: 'By default, halos only cycle from front-most submorph to parent (shift key down). This setting enables halos to also cycle from top-level morph down through the submorphs (no shift key)'.
			(aBuilder setting: #haloEnclosesFullBounds)
				label: 'Encloses fullbounds';
				default: false;
				target: HaloMorph;
				description: 'If true, halos will enclose the full bounds of the target Morph, rather than just the bounds'.
			(aBuilder setting: #showBoundsInHalo)
				label: 'Exhibits bounds' ;
				target: HaloMorph;
				default: false;
				description: 'If true, halos will include a fine rectangle around the bounds of the target morph.'
		]
]

{ #category : 'morphic' }
PolymorphSystemSettings class >> morphicMenuSettingsOn: aBuilder [
	(aBuilder group: #menu)
		label: 'Menus';
		parent: #morphic;
		description: 'All menu settings' ;
		target: UITheme;
		targetSelector: #currentSettings;
		noOrdering;
		with: [
			(aBuilder setting: #preferGradientFill)
				label: 'Gradient filling' ;
				default: false;
				description: 'If true, then menus will have a gradient look' .
			(aBuilder setting: #flatMenu)
				label: 'Flat appearance' ;
				default: true;
				description: 'If true, then menu will without 3D like borders.' .
			(aBuilder setting: #autoMenuColor)
				label: 'Computed color' ;
				default: false;
				description: 'If true, then the menu color will be computed from world dressing.' .
			(aBuilder setting: #menuColor)
				label: 'Menu color';
				default: (Color r: 0.823069403714565 g: 0.823069403714565 b: 0.823069403714565 alpha: 1.0);
				description: 'The menu color to use if it is not computed automatically (see the ''Computed color'' setting)' .
			(aBuilder setting: #menuSelectionColor)
				label: 'Menu selection color' ;
				default: (Color r: 0.38025415444770283 g: 0.6383186705767351 b: 0.8817204301075269 alpha: 1.0);
				description: 'The background color of a selected menu item' .
			(aBuilder setting: #menuSelectionTextColor)
				description: 'The text color of a selected menu item' ;
				default: Color black;
				label: 'Menu selection text color' .
		]
]

{ #category : 'morphic' }
PolymorphSystemSettings class >> morphicSettingsOn: aBuilder [
	(aBuilder group: #morphic)
		label: 'Morphic';
		description: 'All morphic settings';
		noOrdering;
		with: [
			self morphicWindowsSettingsOn: aBuilder.
			self morphicMenuSettingsOn: aBuilder.
			self morphicHaloSettingsOn: aBuilder.
			(aBuilder setting: #keyboardFocusOnMouseDown)
				label: 'Keyboard focus on mouse down';
				default: true;
				target: ModelMorph;
				description: 'When enabled the mouse button must be clicked within a morph for it to take the keyboard focus.' .
			(aBuilder setting: #mouseOverForKeyboardFocus)
				label: 'Lose keyboard focus when mouse leave';
				target: ModelMorph;
				default: false;
				description: 'When true, the mouse must be over a text or list pane in morphic for keystrokes to be felt, and when the mouse is out over the desktop, the so-called desktop-command-keys, such as cmd-b and cmd-R, are honored.  When false, list panes and text panes in morphic remain sensitive to keystrokes even after the mouse is no longer over the pane.' .
			(aBuilder setting: #balloonHelpEnabled)
				label: 'Balloon Tooltips';
				description: 'If true, tooltips are shown when leaving the cursor on top of a graphic element';
				default: true;
				target: UITheme;
				targetSelector: #currentSettings.
			(aBuilder setting: #editableStringMorph)
				label: 'String morphs are editable';
				target: StringMorph;
				default: false;
				description: 'If true, editing on StringMorph is possible if the shift key is pressed while selecting the morphs.' .
			(aBuilder setting: #defaultYellowButtonMenuEnabled)
				label: 'Show default action click menu';
				target: Morph;
				default: false;
				description: 'If true, populate an action click menu (default context menu).'.
			(aBuilder setting: #easySelectingWorld)
				label: 'Easy selection';
				default: false;
				target: WorldState;
				description: 'If true, select multiple Morphs in the world by dragging a selection box around them.']
]

{ #category : 'morphic' }
PolymorphSystemSettings class >> morphicWindowsSettingsOn: aBuilder [
	(aBuilder group: #windows)
		label: 'Windows';
		description: 'All windows settings';
		noOrdering;
		with: [
			(aBuilder setting: #preferRoundCorner)
				target: UITheme;
				targetSelector: #currentSettings;
				default: false;
				label: 'Rounded corners';
				description: 'If true, then windows, menu and other popups will have their corners rounded.'.
			(aBuilder range: #fullscreenMargin)
						target: SystemWindow;
						default: 0;
						label: 'Fullscreen margin';
						description: 'Specify the amount of space that is let around a windows when it''s opened fullscreen';
						range: (-5 to: 100).
			(aBuilder setting: #fadedBackgroundWindows)
				target: UITheme;
				targetSelector: #currentSettings;
				default: false;
				label: 'Fade background windows';
				description: 'If true then, background windows appear more "washed out" to distinguish from the active window'.
			(aBuilder setting: #fastDragging)
				target: UITheme;
				targetSelector: #currentSettings;
				label: 'Fast dragging';
				default: false;
				description: 'If true, window drag will be done by dragging an outline of the window instead of the whole window. It is very convenient on a slow machine' .
			(aBuilder range: #textEntryFieldMinimumWidth)
				label: 'Text entry dialog width';
				default: 450;
				target: TextEntryDialogWindow;
				selector: #minimumWidth;
				range: (100 to: 800);
				description: 'Set the default text field width of the text entry dialog window' ]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> pharoLogo [
	^ ImageMorph withFormSet: self pharoLogoFormSet
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> pharoLogoContents [
	^ self pharoLogoContents1
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> pharoLogoContents1 [
	^ '
		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
		YII='
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> pharoLogoContents2 [
	^ '
		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'
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> pharoLogoForm [
	^ self pharoLogoFormSet asForm
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> pharoLogoFormSet [
	^ FormSet forms: ({ self pharoLogoContents1. self pharoLogoContents2 }
		collect: [ :contents | Form fromBinaryStream: contents base64Decoded readStream ])
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> setDesktopLogoWith: aMorph [
	self desktopLogo ifNotNil: [ self desktopLogo delete ].
	self desktopLogo: aMorph.
	self desktopLogo
		ifNotNil: [ :logo |
			logo
				beSticky;
				lock.
			"The logo image should appear just below the menu bar"
			logo position: 10 @ (self currentWorld menubar height + 10).
			self currentWorld backgroundMorph
				ifNil: [ self currentWorld addMorphBack: logo ]
				ifNotNil: [ self currentWorld addMorph: logo inFrontOf: self currentWorld backgroundMorph ] ]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> showDesktopLogo [
	^ ShowDesktopLogo ifNil: [ShowDesktopLogo := true]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> showDesktopLogo: aBoolean [
	ShowDesktopLogo = aBoolean ifTrue: [^ self].
	ShowDesktopLogo := aBoolean.
	self desktopLogoChanged
]

{ #category : 'morphic' }
PolymorphSystemSettings class >> uiThemeClass [
	^ UITheme current class
]

{ #category : 'morphic' }
PolymorphSystemSettings class >> uiThemeClass: aUIThemeClass [
	| themeClass |
	themeClass := aUIThemeClass
		ifNil: [ UITheme standardThemeClass ].
	 themeClass beCurrent
]

{ #category : 'morphic' }
PolymorphSystemSettings class >> uiThemeClassChoices [
	^ UITheme allThemeClasses collect: [:c | c themeName -> c]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> useDesktopGradientFill [
	^ UseDesktopGradientFill ifNil: [UseDesktopGradientFill := false]
]

{ #category : 'desktop' }
PolymorphSystemSettings class >> useDesktopGradientFill: aBoolean [
	UseDesktopGradientFill := aBoolean.
	self desktopBackgroundChanged
]

{ #category : 'morphic' }
PolymorphSystemSettings class >> windowPositionStrategySettingsOn: aBuilder [
	<systemsettings>
	(aBuilder pickOne: #usedStrategy)
		label: 'Window position strategy';
		parent: #windows;
		target: RealEstateAgent;
		default: #cascadeFor:initialExtent:world:;
		domainValues: {'Reverse Stagger' -> #staggerFor:initialExtent:world:. 'Cascade' -> #cascadeFor:initialExtent:world:. 'Standard' -> #standardFor:initialExtent:world:};
		description: 'If Reversed Stagger, windows are placed in free space,
if Cascade, windows are placed in cascade based on current active window or based on cursor position.'
]

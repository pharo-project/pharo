"
Common superclass for User Interface themes.
Provides methods for creating new morphs in a standard way, various ""services"" like a file dialog, message dialogs etc. and also methods for customising aspects of the appearance of various morphs.

Though conceptually abstract, no code is ""missing"". Subclasses, therefore, should override the aspects they wish to change.

## Architecture

In the new implementation now I have a color palette. My color palette is the one responsible for caching the colors to avoid re-allocations. The colors are defined in the UIThemeColorConfigurator class. Each theme, dark and light, have their own color configurator subclass. The palette obtains the colors from there and then caches them using a Dictionary.

## How to create your own theme

If you wish to have another custom theme, First you need to define your own color configurator class, see the comment of the abstract class UIThemeColorConfigurator. This is done by sublcassing UIThemeColorConfigurator and defining the missing abstract methods.
Once you have your color configurator, you need to subclass Me and only re-define my class side method `colorConfiguratorClass` returning the class of your defined color configurator.
"
Class {
	#name : 'UITheme',
	#superclass : 'Model',
	#instVars : [
		'settings',
		'formSets',
		'focusIndicator',
		'windowActiveDropShadowStyle',
		'useScrollbarThumbShadow',
		'colorPalette'
	],
	#classVars : [
		'Builder',
		'Current'
	],
	#category : 'Polymorph-Widgets-Themes',
	#package : 'Polymorph-Widgets',
	#tag : 'Themes'
}

{ #category : 'query' }
UITheme class >> allThemeClasses [
	"Answer the subclasses of the receiver that are considered to be
	concrete (useable as a theme)."

	^(self allSubclasses reject: [:c | c isAbstract]) asSortedCollection: [:a :b |
		a themeName <= b themeName]
]

{ #category : 'query' }
UITheme class >> beCurrent [
	"Make a new instance of the receiver be the current theme."

	self isAbstract ifTrue: [^self error: self name, ' is abstract, send #beCurrent to a subclass.'].
	self current: self newDefault
]

{ #category : 'accessing' }
UITheme class >> builder [
	"Answer a morph that has the TEasilyThemed trait."

	^Builder ifNil: [Builder := self newBuilder. Builder]
]

{ #category : 'cleanup' }
UITheme class >> cleanUp [

	"avoid old references to morphs that should be garbage-collected"
	self current focusIndicator: nil
]

{ #category : 'configuration' }
UITheme class >> colorConfiguratorClass [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
UITheme class >> current [
	"Answer the current ui theme.
	WARNING: Direct access to this method is ill-adviced, use self theme instead."

	^Current ifNil: [
		Current := PharoLightTheme newDefault.
		Current ]
]

{ #category : 'accessing' }
UITheme class >> current: aUITheme [
	"Set the current ui theme."

	Current := aUITheme.
	SystemProgressMorph reset. "reset to use new fill styles"
	ScrollBarMorph initializeImagesCache. "reset to use new arrows"

	self class environment at: #SHPreferences ifPresent: [ :shPreferences | shPreferences setStyleTable: aUITheme shStyleTable ].

	self class environment at: #PolymorphSystemSettings ifPresent: [ :polymorphSystemSettings | polymorphSystemSettings desktopColor: aUITheme desktopColor ].

	self class environment at: #NECPreferences ifPresent: [ :necPreferences | necPreferences backgroundColor: aUITheme windowColor ].

	self class environment at: #Paragraph ifPresent: [ :paragraph | paragraph insertionPointColor: aUITheme caretColor ].

	self class environment at: #BalloonMorph ifPresent: [ :balloonMorph | balloonMorph setBalloonColorTo: aUITheme balloonBackgroundColor ].

	aUITheme updateWorldDockingBars.
	self currentWorld themeChanged.
	(SystemWindow allSubInstances select: [ :e | e owner isNil ]) do: [ :each | each themeChanged ].

	self codeSupportAnnouncer announce: UIThemeChanged
]

{ #category : 'accessing' }
UITheme class >> currentSettings [

	"Answer the current ui theme settings."

	^ self current settings
]

{ #category : 'class initialization' }
UITheme class >> initialize [

	self current class = UITheme
		ifTrue: [self current: nil
			"due to change to being abstract"]
]

{ #category : 'testing' }
UITheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^true
]

{ #category : 'testing' }
UITheme class >> isCurrent [
	"Answer whether an instance of the receiver is the current theme."

	^Current class == self
]

{ #category : 'accessing' }
UITheme class >> newBuilder [
	"Answer a new builder morph."

	^ComposableMorph new
]

{ #category : 'accessing' }
UITheme class >> newDefault [
	"Answer a new default ui theme."

	^self new
]

{ #category : 'class initialization' }
UITheme class >> resetBuilder [
	<script>

	Builder := nil
]

{ #category : 'accessing' }
UITheme class >> standardThemeClass [
	^ PharoLightTheme
]

{ #category : 'accessing' }
UITheme class >> themeName [
	"Answer the friendly name of the theme."

	self subclassResponsibility
]

{ #category : 'services' }
UITheme >> abortIn: aThemedMorph text: aStringOrText title: aString [
	"Answer the result of an error dialog (true) with the given label and title."

	| window |
	window := (ErrorDialogWindow newWithTheme: aThemedMorph theme)
		textFont: self textFont;
		title: aString;
		text: aStringOrText.
	window openModal.
	^ window cancelled not
]

{ #category : 'services' }
UITheme >> alertIn: aThemedMorph text: aStringOrText title: aString configure: aBlock [
	"Answer the result of an alert dialog (true) with the given label and title."

	|dialog|
	dialog := (AlertDialogWindow newWithTheme: aThemedMorph theme)
		textFont: self textFont;
		title: aString;
		text: aStringOrText.
	aBlock value: dialog.
	dialog openModal.
	^dialog cancelled not
]

{ #category : 'accessing' }
UITheme >> animationSettings [
	^ self settings animationSettings
]

{ #category : 'accessing' }
UITheme >> autoMenuColor [
	^ self settings autoMenuColor
]

{ #category : 'accessing - colors' }
UITheme >> backgroundColor [

	^ colorPalette backgroundColor
]

{ #category : 'accessing - colors' }
UITheme >> balloonBackgroundColor [

	^ colorPalette balloonBackgroundColor
]

{ #category : 'accessing - colors' }
UITheme >> balloonTextColor [

	^ colorPalette balloonTextColor
]

{ #category : 'accessing - colors' }
UITheme >> baseColor [

	^ colorPalette baseColor
]

{ #category : 'basic-colors' }
UITheme >> baseColorFor: aWidget [

	^ colorPalette baseColorFor: aWidget
]

{ #category : 'private' }
UITheme >> basePassiveBackgroundColor [

	^ colorPalette basePassiveBackgroundColor
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> baseScrollbarColorFor: aScrollbar [

	"Return the scrollbar last pane colour or that of our settings if unavailable"

	^ colorPalette baseScrollbarColorFor: aScrollbar
]

{ #category : 'accessing - colors' }
UITheme >> baseSelectionColor [

	^ colorPalette baseSelectionColor
]

{ #category : 'private' }
UITheme >> baseSelectionColorFor: aButton [

	^ colorPalette baseSelectionColor
]

{ #category : 'scrollbars' }
UITheme >> basicCreateArrowOfDirection: aSymbolDirection size: finalSizeInteger color: aColor [
	"PRIVATE - create an arrow with aSymbolDirectionDirection,
	finalSizeInteger and aColor.
	aSymbolDirectionDirection = #top, #bottom. #left or #right .
	Try with:
	(UITheme current
		basicCreateArrowOfDirection: #top
		size: 32
		color: Color lightGreen) asMorph openInHand.
	Fixed to work with translucent colours."

	|form resizeFactor arrow resizedForm |
	resizeFactor := 4.
	form := Form
		extent: finalSizeInteger asPoint * resizeFactor
		depth: 32.
	form fillColor: Color transparent.
	arrow := (self createArrowOfDirection: aSymbolDirection in: form boundingBox)
		borderWidth: 1;
		borderColor: aColor lighter.
	self
		updateScrollBarButtonAspect: arrow
		color: (aColor alphaMixed: 0.7 with: Color black).
	arrow fullDrawOn: form getCanvas.
	resizedForm := form
		magnifyBy: 1 / resizeFactor
		smoothing: 4.
	^resizedForm trimBordersOfColor: Color transparent
]

{ #category : 'accessing - colors' }
UITheme >> borderColor [

	^ colorPalette borderColor
]

{ #category : 'accessing' }
UITheme >> borderWidth [
	^ 1 * self displayScaleFactor
]

{ #category : 'accessing' }
UITheme >> builder [
	^ self class builder
]

{ #category : 'accessing - colors' }
UITheme >> buttonColor [

	^ colorPalette buttonColor
]

{ #category : 'basic-colors' }
UITheme >> buttonColorFor: aButton [

	"Answer the colour for the given button."

	^ colorPalette buttonColorFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonColouredMiddleColor [

	^ colorPalette buttonColouredMiddleColor
]

{ #category : 'border-styles - buttons' }
UITheme >> buttonCornerStyleIn: aThemedMorph [
	"Allow for themes to override default behaviour."

	^ #square
]

{ #category : 'border-styles - buttons' }
UITheme >> buttonDisabledBorderStyleFor: aButton [
	"Return the disabled button borderStyle for the given button."

	^self buttonNormalBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonDisabledFillStyleFor: aButton [

	^ SolidFillStyle color: Color transparent
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonFocusBoundsFor: aButton [
	"Answer the bounds for drawing the focus indication for the
	given button."

	^aButton bounds
]

{ #category : 'defaults' }
UITheme >> buttonFocusIndicatorCornerRadiusFor: aButton [
	"Answer the default corner radius preferred for the focus indicator
	for the button for themes that support this."

	^aButton wantsRoundedCorners
		ifTrue: [7]
		ifFalse: [2]
]

{ #category : 'defaults' }
UITheme >> buttonFont [
	"Answer the button font to use."

	^StandardFonts buttonFont
]

{ #category : 'label-styles - buttons' }
UITheme >> buttonLabelFor: aButton [
	"Answer the label to use for the given button."

	|label|
	label := self buttonLabelForText: aButton label.
	label enabled: aButton enabled.
	label font: self buttonFont.
	label extent: label optimalExtent.
	^label
]

{ #category : 'label-styles - buttons' }
UITheme >> buttonLabelForText: aTextOrString [
	"Answer the label to use for the given text."
	^aTextOrString isString
		ifTrue: [(LabelMorph contents: aTextOrString) color: self textColor ]
		ifFalse: [|t|
				t := TextMorph new newContents: aTextOrString.
				t
					extent: t optimalExtent;
					margins: (Margin left: 0 right: 0 top: 0 bottom: 1 scaledByDisplayScaleFactor);
					lock]
]

{ #category : 'defaults' }
UITheme >> buttonLabelInsetFor: aButton [
	"Answer the inset to use for a button's label."

	|left top right bottom|
	^aButton wantsRoundedCorners
		ifTrue: [left := ((aButton roundedCorners includesAnyOf: #(1 2)) ifTrue: [8] ifFalse: [4]) scaledByDisplayScaleFactor.
				right := ((aButton roundedCorners includesAnyOf: #(3 4)) ifTrue: [8] ifFalse: [4]) scaledByDisplayScaleFactor.
				top := bottom := 2 scaledByDisplayScaleFactor.
				Margin left: left top: top right: right bottom: bottom]
		ifFalse: [(3@2) scaledByDisplayScaleFactor]
]

{ #category : 'defaults' }
UITheme >> buttonMinHeight [
	"Answer the minumum height of a button for this theme."

	^ 18 * self displayScaleFactor
]

{ #category : 'defaults' }
UITheme >> buttonMinWidth [
	"Answer the minumum width of a button for this theme."

	^24 scaledByDisplayScaleFactor
]

{ #category : 'border-styles - buttons' }
UITheme >> buttonMouseOverBorderStyleFor: aButton [
	"Return the mouse over button borderStyle for the given button."

	^self buttonNormalBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonMouseOverFillStyleFor: aButton [
	"Return the button mouse over fillStyle for the given color."

	^self buttonNormalFillStyleFor: aButton
]

{ #category : 'border-styles - buttons' }
UITheme >> buttonNormalBorderStyleFor: aButton [
	"Return the normal button borderStyle for the given button."

	^BorderStyle simple
		width: 0;
		baseColor: Color transparent
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonNormalFillStyleFor: aButton [
	"Return the normal button fillStyle for the given button."

	(aButton valueOfProperty: #noFill ifAbsent: [false]) ifTrue: [^ SolidFillStyle color: Color transparent ].
	^ self normalFillStyleFor: aButton height: aButton height
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonPlainMiddleColor [

	^ colorPalette buttonPlainMiddleColor
]

{ #category : 'border-styles - buttons' }
UITheme >> buttonPressedBorderStyleFor: aButton [
	"Return the pressed button borderStyle for the given button."

	^self buttonSelectedBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonPressedFillStyleFor: aButton [
	"Return the button pressed fillStyle for the given button."

	^self buttonSelectedFillStyleFor: aButton
]

{ #category : 'border-styles - buttons' }
UITheme >> buttonSelectedBorderStyleFor: aButton [
	"Return the selected button borderStyle for the given button."

	^self buttonNormalBorderStyleFor: aButton
]

{ #category : 'border-styles - buttons' }
UITheme >> buttonSelectedDisabledBorderStyleFor: aButton [
	"Return the selecteddisabled button borderStyle for the given button."

	^self buttonSelectedBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonSelectedDisabledFillStyleFor: aButton [
	"Return the button selected disabled fillStyle for the given color."

	^self buttonSelectedFillStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonSelectedFillStyleFor: aButton [
	"Return the normal button fillStyle for the given button."

	^ SolidFillStyle color: self lightSelectionColor
]

{ #category : 'border-styles - buttons' }
UITheme >> buttonSelectedMouseOverBorderStyleFor: aButton [
	"Return the selected mouse over button borderStyle for the given button."

	^self buttonSelectedBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonSelectedMouseOverFillStyleFor: aButton [
	"Return the button selected mouse over fillStyle for the given color."

	^self buttonSelectedFillStyleFor: aButton
]

{ #category : 'border-styles - buttons' }
UITheme >> buttonSelectedPressedBorderStyleFor: aButton [
	"Return the selected pressed button borderStyle for the given button."

	^self buttonNormalBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> buttonSelectedPressedFillStyleFor: aButton [
	"Return the button selected pressed fillStyle for the given color."

	^self buttonNormalFillStyleFor: aButton
]

{ #category : 'accessing - colors' }
UITheme >> caretColor [

	^ colorPalette caretColor
]

{ #category : 'services' }
UITheme >> centeredAlertIn: aThemedMorph text: aStringOrText title: aString configure: aBlock [
	"Answer the result of an alert dialog (true) with the given label and title."

	| dialog |
	dialog := (AlertDialogWindow newWithTheme: aThemedMorph theme)
		textFont: self textFont;
		title: aString;
		text: aStringOrText.
	aBlock value: dialog.
	dialog left: ((self currentWorld width / 2) - (dialog width /2)).
	dialog top: ((self currentWorld height / 2) - (dialog height /2)).
	dialog openModal.
	^ dialog cancelled not
]

{ #category : 'border-styles - buttons' }
UITheme >> checkboxButtonDisabledBorderStyleFor: aCheckboxButton [
	"Return the disabled checkbox button borderStyle for the given button."

	^self checkboxButtonNormalBorderStyleFor: aCheckboxButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> checkboxButtonDisabledFillStyleFor: aCheckboxButton [
	"Return the disabled checkbox button fillStyle for the given button."

	^self checkboxButtonNormalFillStyleFor: aCheckboxButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> checkboxButtonLabelNormalFillStyleFor: aCheckboxButton label: aLabel [
	"Return the selected disabled checkbox button fillStyle for the given button."

	^ aLabel color
]

{ #category : 'border-styles - buttons' }
UITheme >> checkboxButtonNormalBorderStyleFor: aChecboxButton [
	"Return the normal checkbox button borderStyle for the given button."

	^BorderStyle simple
		width: 1 scaledByDisplayScaleFactor;
		baseColor: Color transparent
]

{ #category : 'fill-styles - buttons' }
UITheme >> checkboxButtonNormalFillStyleFor: aCheckboxButton [
	"Return the normal checkbox button fillStyle for the given checkbox button."

	^(ImageFillStyle formSet: self checkboxFormSet) origin: aCheckboxButton topLeft
]

{ #category : 'border-styles - buttons' }
UITheme >> checkboxButtonSelectedBorderStyleFor: aCheckboxButton [
	"Return the selected checkbox button borderStyle for the given button."

	^self checkboxButtonNormalBorderStyleFor: aCheckboxButton
]

{ #category : 'border-styles - buttons' }
UITheme >> checkboxButtonSelectedDisabledBorderStyleFor: aCheckboxButton [
	"Return the selected disabled checkbox button borderStyle for the given button."

	^self checkboxButtonDisabledBorderStyleFor: aCheckboxButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> checkboxButtonSelectedDisabledFillStyleFor: aCheckboxButton [
	"Return the selected disabled checkbox button fillStyle for the given button."

	^self checkboxButtonSelectedFillStyleFor:  aCheckboxButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> checkboxButtonSelectedFillStyleFor: aCheckboxButton [
	"Return the selected checkbox button fillStyle for the given checkbox button."

	^(ImageFillStyle formSet: self checkboxSelectedFormSet) origin: aCheckboxButton topLeft
]

{ #category : 'border-styles - buttons' }
UITheme >> checkboxCornerStyleFor: aCheckbox [
	"Answer the corner style for checkbox buttons."

	^#square
]

{ #category : 'forms' }
UITheme >> checkboxForm [

	^self  checkboxFormSet asForm
]

{ #category : 'forms' }
UITheme >> checkboxFormSet [
	"Answer the form set to use for a normal checkbox."

	^self  checkboxUnselectedFormSet
]

{ #category : 'label-styles - buttons' }
UITheme >> checkboxLabelFor: aCheckbox [
	"Answer the label to use for the given checkbox."

	|label|
	label := self checkboxLabelForText: aCheckbox label.
	label enabled: aCheckbox enabled.
	label font: self labelFont.
	^label
]

{ #category : 'label-styles - buttons' }
UITheme >> checkboxLabelForText: aTextOrString [
	"Answer the label to use for the given text."

	|morph|
	morph := aTextOrString isString
		ifTrue: [(LabelMorph contents: aTextOrString)
				disabledStyle: self disabledItemStyle]
		ifFalse: [|t|
				t := TextMorph new newContents: aTextOrString.
				t extent: t optimalExtent].
	^morph
		vResizing: #shrinkWrap;
		hResizing: #spaceFill;
		lock
]

{ #category : 'label-styles - buttons' }
UITheme >> checkboxMarkerForm [
	"Answer the form to use for a checkbox marker."

	^ self iconNamed: #checkboxMarker
]

{ #category : 'fill-styles - buttons' }
UITheme >> checkboxSelectedForm [

	^ self checkboxSelectedFormSet asForm
]

{ #category : 'fill-styles - buttons' }
UITheme >> checkboxSelectedFormSet [
	"Answer the form set to use for a selected checkbox."

	^ self iconFormSetNamed: #checkboxSelected
]

{ #category : 'forms' }
UITheme >> checkboxUnselectedForm [

	^ self checkboxUnselectedFormSet asForm
]

{ #category : 'forms' }
UITheme >> checkboxUnselectedFormSet [
	"Answer the form set to use for a selected checkbox."

	^ self iconFormSetNamed: #checkboxUnselected
]

{ #category : 'services' }
UITheme >> chooseColorIn: aThemedMorph title: aString color: aColor [
	"Answer the result of a color selector dialog with the given title and initial color."

	|dialog|
	dialog := (ColorSelectorDialogWindow newWithTheme: aThemedMorph theme)
		title: aString;
		selectedColor: aColor.
	dialog openModal.
	^ dialog cancelled ifFalse: [ dialog selectedColor ]
]

{ #category : 'services' }
UITheme >> chooseColorIn: aThemedMorph title: aString color: aColor for: aBlock [
	"execute block with result of a color selector dialog with the given title and initial color."

	|d newColor|
	d := (ColorSelectorDialogWindow newWithTheme: aThemedMorph theme)
			title: aString;
			selectedColor: aColor.
	d extent: d initialExtent.
	d announcer when: ColorChanged do: [:ann |
		aBlock value: ann newColor].
	d openModal.

	newColor := d  cancelled
		ifTrue: [aColor]
		ifFalse: [d selectedColor].

	aBlock value: newColor
]

{ #category : 'services' }
UITheme >> chooseDirectoryIn: aThemedMorph title: title path: path [
	"Answer the result of a file dialog with the given title, choosing directories only."

	^ self directoryDialogWindowClass new
		  defaultFolder: (path ifNil: [ StFileSystemModel defaultDirectory ]);
		  title: (title ifNil: [ 'Choose Directory' translated ]);
		  openModal;
		  selectedEntry
]

{ #category : 'services' }
UITheme >> chooseDropListIn: aThemedMorph text: aStringOrText title: aString list: aList [
	"Answer the result of a drop list chooser with the given label, title and list."

	^((ChooseDropListDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: aStringOrText;
			list: aList) openModal selectedItem
]

{ #category : 'services' }
UITheme >> chooseExistingFileReferenceIn: aThemedMorph title: title extensions: exts path: path preview: preview [
	"Answer the result of a file name chooser dialog with the given title, extensions
	path and preview type.
	Answer nil or a filename."

	| dialog |
	
	dialog := self fileDialogWindowClass new.
	exts 
		ifNotNil: [ dialog filter: (StCustomExtensionsFilter extensions: exts) ].
	path 
		ifNotNil: [ dialog nameText: path basename ].
	^ dialog
		defaultFolder: (path ifNil: [ StFileSystemModel defaultDirectory ]);
		title: (title ifNil: [ 'Choose File' translated ]);
		openModal;
		selectedEntry

]

{ #category : 'services' }
UITheme >> chooseFileIn: aThemedMorph title: title extensions: exts path: path preview: preview [
	"Answer the result of a file open dialog with the given title, extensions path and preview type.
	Answer nil or a filename."

	| dialog |
	
	dialog := self fileDialogWindowClass new.
	exts 
		ifNotNil: [ dialog filter: (StCustomExtensionsFilter extensions: exts) ].
	path 
		ifNotNil: [ dialog nameText: path basename ].
	^ dialog
		defaultFolder: (path ifNil: [ StFileSystemModel defaultDirectory ]);
		title: (title ifNil: [ 'Choose File' translated ] ifNotNil: [ title ]);
		openModal;
		selectedEntry

]

{ #category : 'services' }
UITheme >> chooseForSaveFileReferenceIn: aThemedMorph title: title extensions: exts path: path preview: preview [
	"Answer the result of a file name chooser dialog with the given title, extensions
	path and preview type.
	Answer nil or a filename."

	| dialog directory |
	dialog := self fileSaveDialogWindowClass new.
	exts ifNotNil: [
		dialog filter: (StCustomExtensionsFilter extensions: exts) ].
	path ifNotNil: [
		dialog nameText: path asFileReference nextVersion basename ].
	directory := path
		             ifNil: [ StFileSystemModel defaultDirectory ]
		             ifNotNil: [ path parent ].
	^ dialog
		  defaultFolder: directory;
		  title: (title ifNil: [ 'Save as' translated ] ifNotNil: [ title ]);
		  openModal;
		  selectedEntry
]

{ #category : 'services' }
UITheme >> chooseFullFileNameIn: aThemedMorph title: title patterns: patterns path: path preview: preview [
	"Answer the result of a file name chooser dialog with the given title, patterns
	path and preview type.
	Answer nil or a filename."

	| dialog |
	
	dialog := self fileDialogWindowClass new.
	path 
		ifNotNil: [ dialog nameText: path basename ].
	^ dialog
		defaultFolder: (path ifNil: [ StFileSystemModel defaultDirectory ]);
		title: (title ifNil: [ 'Choose File' translated ]);
		openModal;
		selectedEntry.
	

]

{ #category : 'services' }
UITheme >> chooseIn: aThemedMorph title: title labels: labels values: values lines: lines [
	"Answer the result of a popup choice with the given title, labels, values and lines."

	| dialog |
	dialog := (PopupChoiceDialogWindow newWithTheme: aThemedMorph theme)
		title: (title isEmpty ifTrue: ['Choose' translated] ifFalse: [title asString]);
		labels: labels;
		lines: (lines ifNil: [#()]);
		model: values.
	^ dialog openModal choice
]

{ #category : 'services' }
UITheme >> chooseIn: aThemedMorph title: title message: aMessage labels: labels values: values lines: lines [
	"Answer the result of a popup choice with the given title, labels, values and lines."

	|dialog|
	dialog := (PopupChoiceDialogWindowWithMessage newWithTheme: aThemedMorph theme)
		title: (title isEmpty ifTrue: ['Choose' translated] ifFalse: [title asString]);
		textFont: self textFont;
		message: aMessage;
		labels: labels;
		lines: (lines ifNil: [#()]);
		model: values.
	^dialog openModal choice
]

{ #category : 'services' }
UITheme >> chooseOrRequestIn: aThemedMorph title: title labels: labels values: values lines: lines [
	"Answer the result of a popup choice with the given title, labels, values and lines."

	| dialog choice |
	dialog := (PopupChoiceOrRequestDialogWindow newWithTheme: aThemedMorph theme)
		title: (title ifEmpty: [ 'Choose' translated ] ifNotEmpty: #asString);
		labels: labels;
		lines: (lines ifNil: [ #() ]);
		model: values.
	choice := dialog openModal choice.
	dialog cancelled ifTrue: [ ^ nil ].
	^ choice ifNil: [ dialog filterValue ]
]

{ #category : 'accessing - colors' }
UITheme >> classExtensionColor [

	^ colorPalette classExtensionColor
]

{ #category : 'label-styles' }
UITheme >> configureDialogWindowLabelAreaFrameFor: aWindow [
	"Configure the layout frame for the label area for the given dialog window."

	|frame|
	self configureWindowLabelAreaFrameFor: aWindow.
	aWindow labelArea ifNil: [^ self].
	frame := aWindow labelArea layoutFrame.
	frame
		leftOffset: 0;
		rightOffset: 0
]

{ #category : 'border-styles' }
UITheme >> configureWindowBorderFor: aWindow [
	"Configure the border for the given window."

	aWindow roundedCorners: #(1 2 3 4).
	aWindow borderStyle: (BorderStyle
		width: 1 scaledByDisplayScaleFactor color: Color lightGray)
]

{ #category : 'border-styles' }
UITheme >> configureWindowDropShadowFor: aWindow [

	aWindow hasDropShadow: false
]

{ #category : 'label-styles' }
UITheme >> configureWindowLabelAreaFor: aWindow [
	"Configure the label area for the given window."

	| macOSLayout addCloseBox addCollapseExpandBoxes addMenuBox padding paddingStep |
	macOSLayout := self labelAreaLayout = #macOS
		or: [ self labelAreaLayout = #platformDependent and: Smalltalk os isMacOS ].
	addCloseBox := [ aWindow hasCloseBox ifTrue: [aWindow addCloseBox ] ].
	addCollapseExpandBoxes := [
		aWindow hasCollapseBox ifTrue: [aWindow addCollapseBox ].
		aWindow hasExpandBox ifTrue: [aWindow addExpandBox ] ].
	addMenuBox := [ aWindow hasMenuBox ifTrue: [aWindow addMenuControl ] ].
	padding := 0.
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0).
	macOSLayout
		ifTrue: [ addCloseBox value. addCollapseExpandBoxes value. paddingStep := 1 ]
		ifFalse: [ addMenuBox value. paddingStep := -1 ].
	aWindow hasCloseBox ifTrue: [padding := padding + paddingStep].
	aWindow hasCollapseBox ifTrue: [padding := padding + paddingStep].
	aWindow hasExpandBox ifTrue: [padding := padding + paddingStep].
	aWindow hasMenuBox ifTrue: [padding := padding - paddingStep].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0; hResizing: #spaceFill).
	aWindow basicLabel ifNotNil: [:label |
		label color: self textColor.
		aWindow labelArea
			addMorphBack: label;
			hResizing: #shrinkWrap ].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0; hResizing: #spaceFill).
	padding > 0 ifTrue: [
		aWindow labelArea
			addMorphBack: (Morph new extent: (aWindow boxExtent x * padding) @ 0)].
	macOSLayout
		ifTrue: [ addMenuBox value ]
		ifFalse: [ addCollapseExpandBoxes value. addCloseBox value ].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0)
]

{ #category : 'label-styles' }
UITheme >> configureWindowLabelAreaFrameFor: aWindow [
	"Configure the layout frame for the label area for the given window."

	|frame windowBorderWidth|
	aWindow labelArea ifNil: [^ self].
	windowBorderWidth := aWindow class borderWidth.
	aWindow labelArea
		layoutPolicy: RowLayout new;
		cellPositioning: #topCenter;
		hResizing: #spaceFill;
		wrapCentering: #topLeft;
		layoutInset: (Margin left: 0 top: windowBorderWidth right: 0 bottom: 1 scaledByDisplayScaleFactor).
	frame := LayoutFrame identity bottomFraction: 0 ; topOffset: aWindow labelHeight negated.
	aWindow labelArea layoutFrame: frame
]

{ #category : 'border-styles - buttons' }
UITheme >> controlButtonDisabledBorderStyleFor: aButton [
	"Return the disabled control button borderStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self buttonDisabledBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> controlButtonDisabledFillStyleFor: aButton [
	"Return the disabled control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self controlButtonSelectedFillStyleFor: aButton
]

{ #category : 'defaults' }
UITheme >> controlButtonLabelInsetFor: aButton [
	"Answer the inset to use for a control button's label."

	^2 scaledByDisplayScaleFactor
]

{ #category : 'border-styles - buttons' }
UITheme >> controlButtonMouseOverBorderStyleFor: aButton [
	"Return the mouse over control button borderStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self buttonMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> controlButtonMouseOverFillStyleFor: aButton [
	"Return the mouse over control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self controlButtonNormalFillStyleFor: aButton
]

{ #category : 'border-styles - buttons' }
UITheme >> controlButtonNormalBorderStyleFor: aButton [
	"Return the normal control button borderStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self buttonNormalBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> controlButtonNormalFillStyleFor: aButton [
	"Return the normal control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self buttonSelectedFillStyleFor: aButton
]

{ #category : 'border-styles - buttons' }
UITheme >> controlButtonPressedBorderStyleFor: aButton [
	"Return the pressed control button borderStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self buttonPressedBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> controlButtonPressedFillStyleFor: aButton [
	"Return the pressed button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self controlButtonSelectedFillStyleFor: aButton
]

{ #category : 'border-styles - buttons' }
UITheme >> controlButtonSelectedDisabledBorderStyleFor: aButton [
	"Return the selected disabled control button borderStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self buttonSelectedDisabledBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> controlButtonSelectedDisabledFillStyleFor: aButton [
	"Return the selected disabled control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self controlButtonSelectedFillStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> controlButtonSelectedFillStyleFor: aButton [
	"Return the selected control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self buttonNormalFillStyleFor: aButton
]

{ #category : 'border-styles - buttons' }
UITheme >> controlButtonSelectedMouseOverBorderStyleFor: aButton [
	"Return the selected mouse over control button borderStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self buttonSelectedMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> controlButtonSelectedMouseOverFillStyleFor: aButton [
	"Return the selected mouse over control button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self controlButtonSelectedFillStyleFor: aButton
]

{ #category : 'border-styles - buttons' }
UITheme >> controlButtonSelectedPressedBorderStyleFor: aButton [
	"Return the selected pressed control button borderStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self buttonSelectedPressedBorderStyleFor: aButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> controlButtonSelectedPressedFillStyleFor: aButton [
	"Return the selected pressed button fillStyle for the given button.
	Control buttons are generally used for drop-lists and expanders."

	^self controlButtonNormalFillStyleFor: aButton
]

{ #category : 'scrollbars' }
UITheme >> createArrowImagesCache [
	"Creates the cache to store the arrow forms"

	^ LRUCache new
		maximumWeight: 40;
		factory: [ :key |
			| form1 form2 |
			form1 := self scrollbarArrowOfDirection: key first
				size: key second
				color: key third.
			form2 := self scrollbarArrowOfDirection: key first
				size: key second * 2
				color: key third.
			FormSet forms: {
				form1.
				form2 scaledToExactSize: form1 extent * 2 } ];
		yourself
]

{ #category : 'scrollbars' }
UITheme >> createArrowOfDirection: aSymbol in: aRectangle [
	"PRIVATE - create an arrow bounded in aRectangle"

	| arrow vertices |
	vertices := self scrollBarButtonArrowVertices: aRectangle.
	""
	arrow := PolygonMorph
				vertices: vertices
				color: Color transparent
				borderWidth: 0
				borderColor: Color black.
	""
	arrow bounds: (arrow bounds insetBy: (aRectangle width / 6) rounded).
	""
	aSymbol == #right
		ifTrue: [arrow rotationDegrees: arrow rotationDegrees + 90].
	aSymbol == #bottom
		ifTrue: [arrow rotationDegrees: arrow rotationDegrees + 180].
	aSymbol == #left
		ifTrue: [arrow rotationDegrees: arrow rotationDegrees + 270].
	""
	^arrow
]

{ #category : 'scrollbars' }
UITheme >> createBoxImagesCache [
	"Creates the cache to store the arrow forms"

	^ LRUCache new
		maximumWeight: 20;
		factory: [ :key |
			self createBoxOfSize: key first color: key second ];
		yourself
]

{ #category : 'scrollbars' }
UITheme >> createBoxIn: aRectangle [
	"PRIVATE - create an box bounded in aRectangle"
	| box |
	box := Morph new.
	box extent: (aRectangle scaleBy: 1 / 2) extent rounded;
		 borderWidth: 0.
	""
	^ box
]

{ #category : 'scrollbars' }
UITheme >> createBoxOfSize: finalSizeInteger color: aColor [
	"PRIVATE - create a box with finalSizeInteger and aColor

	Try with:
	(UITheme current createBoxOfSize: 32 color: Color lightGreen)
          asMorph openInHand.
	"
	| resizeFactor outerBox innerBox resizedForm |
	resizeFactor := 4.
	outerBox := Morph new.
	outerBox extent: finalSizeInteger asPoint * resizeFactor;
		 color: aColor.
	""
	innerBox := self createBoxIn: outerBox bounds.
	self updateScrollBarButtonAspect: innerBox color: aColor muchDarker.
	outerBox addMorphCentered: innerBox.
	""
	resizedForm := outerBox imageForm
				magnifyBy: 1 / resizeFactor
				smoothing: 4.
	""
	^ (resizedForm replaceColor: aColor withColor: Color transparent)
		trimBordersOfColor: Color transparent
]

{ #category : 'label-styles' }
UITheme >> createCloseBoxFor: aSystemWindow [
	"Answer a button for closing the window."

	| announcement |
	^ (self
		   newCloseControlIn: aSystemWindow
		   for: aSystemWindow
		   action: [
			   SystemWindow useHideForClose
				   ifTrue: [
					   aSystemWindow hide.
					   announcement := WindowClosed new
						                   window: aSystemWindow;
						                   yourself.
					   self currentWorld announcer announce: announcement ]
				   ifFalse: [ aSystemWindow closeBoxHit ] ]
		   help: 'Close this window' translated) extent:
		  aSystemWindow boxExtent
]

{ #category : 'label-styles' }
UITheme >> createCollapseBoxFor: aSystemWindow [
	"Answer a button for minimising the window."

	|formSet msb|
	formSet := self windowMinimizeFormSet.
	msb := MultistateButtonMorph new extent: formSet extent.
	msb activeEnabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowMinimizePassiveFormSet.
	msb extent: formSet extent.
	msb passiveEnabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowMinimizeOverFormSet.
	msb extent: formSet extent.
	msb
		activeEnabledOverUpFillStyle: (ImageFillStyle formSet: formSet);
		passiveEnabledOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowMinimizePassiveFormSet.
	msb
		extent: formSet extent;
		activeEnabledOverDownFillStyle: (ImageFillStyle formSet: formSet);
		passiveEnabledOverDownFillStyle: (ImageFillStyle formSet: formSet);
		addUpAction: [aSystemWindow collapseBoxHit];
		setBalloonText: 'Collapse this window' translated;
		extent: aSystemWindow boxExtent.
	^msb
]

{ #category : 'label-styles' }
UITheme >> createExpandBoxFor: aSystemWindow [
	"Answer a button for maximising/restoring the window."

	|formSet msb|
	formSet := self windowMaximizeFormSet.
	msb := MultistateButtonMorph new extent: formSet extent.
	msb activeEnabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowMaximizePassiveFormSet.
	msb extent: formSet extent.
	msb passiveEnabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowMaximizeOverFormSet.
	msb extent: formSet extent.
	msb
		activeEnabledOverUpFillStyle: (ImageFillStyle formSet: formSet);
		passiveEnabledOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowMaximizePassiveFormSet.
	msb
		extent: formSet extent;
		activeEnabledOverDownFillStyle: (ImageFillStyle formSet: formSet);
		passiveEnabledOverDownFillStyle: (ImageFillStyle formSet: formSet);
		addUpAction: [aSystemWindow expandBoxHit];
		setBalloonText: 'Expand to full screen' translated;
		extent: aSystemWindow boxExtent.
	^msb
]

{ #category : 'label-styles' }
UITheme >> createMenuBoxFor: aSystemWindow [
	"Answer a button for the window menu."

"	^aSystemWindow createBox
		labelGraphic: (self windowMenuIconFor: aSystemWindow);
		extent: aSystemWindow boxExtent;
		actWhen: #buttonDown;
		actionSelector: #offerWindowMenu;
		setBalloonText: 'window menu' translated"

	|formSet msb|
	formSet := self windowMenuFormSet.
	msb := MultistateButtonMorph new extent: formSet extent.
	msb activeEnabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowMenuPassiveFormSet.
	msb extent: formSet extent.
	msb activeDisabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	msb passiveEnabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	msb passiveDisabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowMenuFormSet.
	msb extent: formSet extent.
	msb
		activeEnabledOverUpFillStyle: (ImageFillStyle formSet: formSet);
		passiveEnabledOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowMenuPassiveFormSet.
	msb
		extent: formSet extent;
		activeEnabledOverDownFillStyle: (ImageFillStyle formSet: formSet);
		passiveEnabledOverDownFillStyle: (ImageFillStyle formSet: formSet);
		addUpAction: [aSystemWindow offerWindowMenu];
		setBalloonText: 'window menu' translated;
		extent: aSystemWindow boxExtent.
	^msb
]

{ #category : 'accessing' }
UITheme >> currentSettings [

	^ settings
]

{ #category : 'services' }
UITheme >> customQuestionIn: aThemedMorph text: labelText yesText: yesText noText: noText cancelText: cancelText default: defaultOption title: aString [
	"Answer the result of a question dialog with the given label, button labels and title.
	defaultOption should be one of true, false or nil to set the default button."

	^ ((CustomQuestionDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: labelText;
		   yesText: yesText help: nil;
		   noText: noText help: nil;
		   cancelText: cancelText help: nil;
		   default: defaultOption) openModal answer
]

{ #category : 'accessing - colors' }
UITheme >> dangerBackgroundColor [

	^ colorPalette dangerBackgroundColor
]

{ #category : 'accessing - colors' }
UITheme >> dangerTextColor [

	^ colorPalette dangerTextColor
]

{ #category : 'accessing - colors' }
UITheme >> darkBaseColor [

	^ colorPalette darkBaseColor
]

{ #category : 'private' }
UITheme >> darkBaseColorFor: aButton [

	^ self darkBaseColor
]

{ #category : 'defaults' }
UITheme >> defaultButtonBorderColor [
	"Answer the color to use for a 'default' button."

	^self settings selectionColor
]

{ #category : 'defaults' }
UITheme >> defaultMenuColor [
	"Answer the default color to use for a menu
	in the absence of an override in settings."

	^ self settings fallbackMenuColor
]

{ #category : 'defaults' }
UITheme >> defaultMenuTitleColor [
	"Answer the default color to use for a menu title
	in the absence of an override in settings."

	^self settings menuTitleColor
]

{ #category : 'initialization' }
UITheme >> defaultSettings [

	^ ThemeSettings new
		buttonColor: Color gray;
		scrollbarColor: Color gray;
		menuColor: self baseColor;
		menuTitleColor: self baseColor;
		windowColor: self baseColor;
		selectionColor: self lightSelectionColor;
		menuSelectionColor: self baseSelectionColor;
		progressBarColor: self baseColor;
		standardColorsOnly: true;
		autoSelectionColor: false;
		preferRoundCorner: false;
		fadedBackgroundWindows: false;
		flatMenu: true;
		yourself
]

{ #category : 'services' }
UITheme >> denyIn: aThemedMorph text: aStringOrText title: aString [
	"Answer the result of an deny dialog (true) with the given label and title."

	^ ((DenyDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText) openModal cancelled not
]

{ #category : 'fill-styles' }
UITheme >> desktopColor [

	^ colorPalette desktopColor
]

{ #category : 'fill-styles' }
UITheme >> desktopImageFor: aWorld [
	"Answer the desktop image for the given world.
	Look for an image file with the name of the theme.
	Answer nil for none available."

	|file|
	file := FileSystem disk workingDirectory / self class themeName , 'jpg'.
	^ file exists ifTrue: [
		ImageReadWriter formFromFileNamed: file fullName]
]

{ #category : 'fill-styles' }
UITheme >> desktopImageLayout [
	"Answer the layout symbol for any desktop image.
	See AlphaImageMorph>>layoutSymbols."

	^self settings desktopImageLayout
]

{ #category : 'fill-styles' }
UITheme >> dialogWindowActiveFillStyleFor: aWindow [
	"Return the window active fillStyle for the given window."

	^ self windowActiveFillStyle
]

{ #category : 'fill-styles' }
UITheme >> dialogWindowInactiveFillStyleFor: aWindow [
	"Return the window active fillStyle for the given window."

	^self windowInactiveFillStyleFor: aWindow
]

{ #category : 'defaults' }
UITheme >> dialogWindowPreferredCornerStyleFor: aDialogWindow [
	"Answer the preferred corner style for the given dialog."

	^ self windowPreferredCornerStyleFor:aDialogWindow
]

{ #category : 'basic-colors - diffs' }
UITheme >> diffAdditionalLineBackgroundColor [

	"Answer the color used to show additions in a diff view."

	^ colorPalette diffAdditionalLineBackgroundColor
]

{ #category : 'basic-colors - diffs' }
UITheme >> diffAdditionalTextInLineBackgroundColor [

	"Answer the color used to show addition highlights in a text line."

	^ colorPalette diffAdditionalTextInLineBackgroundColor
]

{ #category : 'basic-colors - diffs' }
UITheme >> diffEdgeColor [

	"Answer the color used to show the border of the changes."

	^ colorPalette diffEdgeColor
]

{ #category : 'basic-colors - diffs' }
UITheme >> diffJoinColor [

	"Answer the color used for the join bar."

	^ colorPalette diffJoinColor
]

{ #category : 'basic-colors - diffs' }
UITheme >> diffMapColor [

	"Answer the color used for the mapping bar on the left side."

	^ colorPalette diffMapColor
]

{ #category : 'basic-colors - diffs' }
UITheme >> diffModificatedLineBackgroundColor [

	"Answer the color used to show changes."

	^ colorPalette diffModificatedLineBackgroundColor
]

{ #category : 'basic-colors - diffs' }
UITheme >> diffRemovalHighlightColor [

	"Answer the color used to show removal highligths."

	^ colorPalette diffRemovalHighlightColor
]

{ #category : 'basic-colors - diffs' }
UITheme >> diffRemovedLinesBackgroundColor [

	"Answer the color used to show removals in a diff view."

	^ colorPalette diffRemovedLinesBackgroundColor
]

{ #category : 'services' }
UITheme >> directoryDialogWindowClass [

	^ self class environment 
		at: #StOpenDirectoryPresenter
		ifAbsent: [  self error: 'Directory dialog support is missing in the image' ]
]

{ #category : 'accessing - colors' }
UITheme >> disabledColor [

	^ colorPalette disabledColor
]

{ #category : 'label-styles' }
UITheme >> disabledItemStyle [
	"Answer either #plain or #inset to determine how
	diabled text is drawn."

	^#plain
]

{ #category : 'basic-colors' }
UITheme >> disabledLabelColor [

	^ colorPalette disabledLabelColor
]

{ #category : 'accessing - colors' }
UITheme >> disabledTextColor [

	^ colorPalette disabledTextColor
]

{ #category : 'accessing' }
UITheme >> displayScaleFactor [

	^ self currentWorld displayScaleFactor
]

{ #category : 'accessing' }
UITheme >> dockingBarBorderWidth [
	^ 0
]

{ #category : 'fill-styles' }
UITheme >> dockingBarNormalFillStyleFor: aToolDockingBar [
	^ SolidFillStyle color: Color transparent
]

{ #category : 'border-styles' }
UITheme >> drawTabGroupFinishingFor: aTabGroupMorph on: aCanvas [
	"Patch up any visuals for the selected tab."
]

{ #category : 'border-styles' }
UITheme >> drawTextAdornmentFor: aPluggableTextMorph color: aColor on: aCanvas [
	"Indicate edit status for the given morph."

	|bounds size fillStyle|
	bounds := aPluggableTextMorph innerBounds.
	size := 25 scaledByDisplayScaleFactor.
	fillStyle := (GradientFillStyle ramp: {
			0.0->(self backgroundColor alpha: 0.01).
			0.8->aColor.
			1.0->aColor})
		origin: bounds topRight - (size@0);
		direction: (size @ size negated) // 4;
		radial: false.
	aCanvas
		drawPolygon:  {bounds topRight. bounds topRight + (0@size). bounds topRight - (size@0)}
		fillStyle: fillStyle
]

{ #category : 'border-styles' }
UITheme >> drawWindowActiveDropShadowFor: aSystemWindow on: aCanvas [
	"Draw the active drop shadow for the given window."


	^self windowActiveDropShadowStyle == #diffuse
		ifTrue: [self drawWindowDiffuseDropShadowFor: aSystemWindow on: aCanvas]
		ifFalse: [self drawWindowPlainDropShadowFor: aSystemWindow on: aCanvas]
]

{ #category : 'border-styles' }
UITheme >> drawWindowDiffuseDropShadowFor: aSystemWindow on: aCanvas [
	"Draw the diffuse drop shadow for the given window."

	|or b r o gc|
	or := self windowActiveDropShadowOffsetRectangleFor: aSystemWindow.
	o := or bottom.
	(aSystemWindow roundedCorners includesAnyOf: #(2 3))
		ifTrue: [o := o + 2 scaledByDisplayScaleFactor].
	b := aSystemWindow bounds expandBy: or.
	gc := {Color black alpha: 0.6. Color transparent}.
	r := b topLeft extent: o asPoint.
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r bottomRight;
					direction: o@0;
					radial: true).
	r := b topLeft + (o@0) extent: (b width - o - o @ o).
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r bottomLeft;
					direction: 0@o negated).
	r := b topRight - (o@0) extent: o asPoint.
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r bottomLeft;
					direction: o@0;
					radial: true).
	r := b topRight + (o negated@o) extent: (o @ (b height - o - o)).
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topLeft;
					direction: o@0).
	r := b bottomRight - o asPoint extent: o asPoint.
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topLeft;
					direction: o@0;
					radial: true).
	r := b bottomLeft + (o@o negated) extent: (b width - o - o @ o).
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topLeft;
					direction: 0@o).
	r := b bottomLeft - (0@o) extent: o asPoint.
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topRight;
					direction: o@0;
					radial: true).
	r := b topLeft + (0@o) extent: (o @ (b height - o - o)).
	aCanvas
		fillRectangle: r
		fillStyle: ((GradientFillStyle colors: gc)
					origin: r topRight;
					direction: o negated@0)
]

{ #category : 'border-styles' }
UITheme >> drawWindowInactiveDropShadowFor: aSystemWindow on: aCanvas [
	"Draw the inactive drop shadow for the given window."

	self drawWindowPlainDropShadowFor: aSystemWindow on: aCanvas
]

{ #category : 'border-styles' }
UITheme >> drawWindowPlainDropShadowFor: aSystemWindow on: aCanvas [
	"Draw the plain drop shadow for the given window."

	|blOffset brOffset trOffset|
	(aSystemWindow owner isNotNil and: [aSystemWindow owner isHandMorph])
		ifTrue: [^self]. "skip if being dragged"
	blOffset := (aSystemWindow wantsRoundedCorners and: [aSystemWindow roundedCorners includes: 2])
		ifTrue: [7 scaledByDisplayScaleFactor] ifFalse: [1 scaledByDisplayScaleFactor].
	brOffset := (aSystemWindow wantsRoundedCorners and: [aSystemWindow roundedCorners includes: 3])
		ifTrue: [7 scaledByDisplayScaleFactor] ifFalse: [1 scaledByDisplayScaleFactor].
	trOffset := (aSystemWindow wantsRoundedCorners and: [aSystemWindow roundedCorners includes: 4])
		ifTrue: [7 scaledByDisplayScaleFactor] ifFalse: [1 scaledByDisplayScaleFactor].
	aCanvas
		fillRectangle: (aSystemWindow bounds bottomLeft + (blOffset @ 0)
						corner: aSystemWindow bounds bottomRight - (brOffset @ -1 scaledByDisplayScaleFactor))
		fillStyle: aSystemWindow shadowColor;
		fillRectangle: (aSystemWindow bounds topRight + (0 @ trOffset)
						corner: aSystemWindow bounds bottomRight + (1 scaledByDisplayScaleFactor @ brOffset negated))
		fillStyle: aSystemWindow shadowColor.
	aCanvas
		clipBy: (aSystemWindow bounds bottomRight - brOffset
			corner: aSystemWindow bounds bottomRight + (1@1) scaledByDisplayScaleFactor)
			during: [:c | ((RoundedBorderStyle new
							baseColor: aSystemWindow shadowColor;
							width: 1 scaledByDisplayScaleFactor) cornerRadius: brOffset - 1 scaledByDisplayScaleFactor)
							frameRectangle: (aSystemWindow bounds translateBy: 1 scaledByDisplayScaleFactor)
							on: c]
]

{ #category : 'label-styles' }
UITheme >> dropListButtonLabelFor: aDropList [
	"Answer the label for the button of the given drop list."

	^AlphaImageMorph new
		formSet: (ScrollBarMorph
			arrowOfDirection: #bottom
			size: aDropList buttonWidth - 3 scaledByDisplayScaleFactor
			color: aDropList paneColor darker);
		enabled: aDropList enabled
]

{ #category : 'border-styles - buttons' }
UITheme >> dropListControlButtonWidth [
	"Answer the width of a drop list control button for this theme."

	^ 18 scaledByDisplayScaleFactor
]

{ #category : 'border-styles' }
UITheme >> dropListDisabledBorderStyleFor: aDropList [
	"Return the disabled borderStyle for the given drop list."

	^self dropListNormalBorderStyleFor: aDropList
]

{ #category : 'fill-styles' }
UITheme >> dropListDisabledFillStyleFor: aDropList [
	"Return the disabled fillStyle for the given drop list."

	^ self textEditorDisabledFillStyleFor: aDropList
]

{ #category : 'fill-styles - buttons' }
UITheme >> dropListFocusBoundsFor: aDropList [
	"Answer the bounds for drawing the focus indication for the
	given drop list."

	^aDropList bounds
]

{ #category : 'defaults' }
UITheme >> dropListFocusIndicatorCornerRadiusFor: aDropList [
	"Answer the default corner radius preferred for the focus indicator
	for the drop list for themes that support this."

	^aDropList wantsRoundedCorners
		ifTrue: [7 scaledByDisplayScaleFactor]
		ifFalse: [2 scaledByDisplayScaleFactor]
]

{ #category : 'defaults' }
UITheme >> dropListFont [
	"Answer the drop list font to use."

	^self textFont
]

{ #category : 'defaults' }
UITheme >> dropListInsetFor: aDropList [
	"Answer the inset to use for drop-list layout."

	^ Margin left: 6 scaledByDisplayScaleFactor top: 0 right: 0 bottom: 0
]

{ #category : 'border-styles' }
UITheme >> dropListNormalBorderStyleFor: aDropList [
	"Return the normal borderStyle for the given drop list"

	^ self buttonNormalBorderStyleFor: aDropList
]

{ #category : 'fill-styles' }
UITheme >> dropListNormalFillStyleFor: aDropList [
	"Return the normal fillStyle for the given drop list."

	^ colorPalette dropListNormalFillStyleFor: aDropList
]

{ #category : 'border-styles' }
UITheme >> dropListNormalListBorderStyleFor: aDropList [
	"Return the normal borderStyle for the list of the given given drop list"

	^BorderStyle inset
		width: 1 scaledByDisplayScaleFactor;
		baseColor: aDropList paneColor
]

{ #category : 'fill-styles' }
UITheme >> dropListNormalListFillStyleFor: aDropList [
	"Return the normal fillStyle for the list of the given drop list."

	^ colorPalette dropListNormalListFillStyleFor: aDropList
]

{ #category : 'accessing - colors' }
UITheme >> enabledColor [

	^ colorPalette enabledColor
]

{ #category : 'services' }
UITheme >> enterOrRequestIn: aThemedMorph title: title labels: labels values: values lines: lines [
	"Answer the result of a popup choice with the given title, labels, values and lines."

	| dialog choice |
	dialog := (PopupChoiceOrRequestDialogWindow newWithTheme: aThemedMorph theme)
		title: (title isEmpty ifTrue: ['Choose' translated] ifFalse: [title asString]);
		labels: labels;
		lines: (lines ifNil: [#()]);
		model: values.
	choice := dialog openModal choice.
	dialog cancelled ifTrue: [ ^nil ].
	^ choice
		ifNotNil: [ choice ]
		ifNil: [ dialog filterValue ]
]

{ #category : 'basic-colors' }
UITheme >> errorPopperBackgroundColor [

	^ colorPalette errorPopperBackgroundColor
]

{ #category : 'border-styles - buttons' }
UITheme >> expanderTitleControlButtonWidth [
	"Answer the width of an expander title control button for this theme."

	^20 scaledByDisplayScaleFactor
]

{ #category : 'defaults' }
UITheme >> expanderTitleInsetFor: aDropList [
	"Answer the inset to use for expander title layout."

	^0
]

{ #category : 'fill-styles' }
UITheme >> expanderTitleNormalFillStyleFor: anExpanderTitle [
	"Return the normal expander title fillStyle for the given expander title."

	^ self buttonNormalFillStyleFor: anExpanderTitle
]

{ #category : 'fill-styles - buttons' }
UITheme >> fadeCheckboxWhenDisabled [
	"Answer whether a checkbox should fade its appearance when disabled.
	If false, assumed that is covered by the relevant disabled fill styles."

	^true
]

{ #category : 'accessing' }
UITheme >> fadedBackgroundWindows [
	^ self settings fadedBackgroundWindows
]

{ #category : 'services' }
UITheme >> fileDialogWindowClass [

	^ self class environment 
		at: #StOpenFilePresenter
		ifAbsent: [  self error: 'File dialog support is missing in the image' ]
]

{ #category : 'services' }
UITheme >> fileSaveDialogWindowClass [

	^ self class environment 
		at: #StSaveFilePresenter
		ifAbsent: [  self error: 'File dialog support is missing in the image' ]
]

{ #category : 'accessing' }
UITheme >> flatMenu [
	^ self settings flatMenu
]

{ #category : 'accessing' }
UITheme >> focusIndicator [
	"Answer the value of focusIndicator"

	^ focusIndicator
]

{ #category : 'accessing' }
UITheme >> focusIndicator: anObject [
	"Set the value of focusIndicator"

	focusIndicator := anObject
]

{ #category : 'defaults' }
UITheme >> focusIndicatorCornerRadiusFor: aMorph [
	"Answer the default corner radius preferred for the focus indicator
	for the morph for themes that support this."

	^aMorph wantsRoundedCorners
		ifTrue: [6 scaledByDisplayScaleFactor]
		ifFalse: [2 scaledByDisplayScaleFactor]
]

{ #category : 'morph creation' }
UITheme >> focusIndicatorMorphFor: aMorph [
	"Answer a (cached) focus indicator for the given morph."

	(self focusIndicator isNil or: [
			self focusIndicator isMorph
				ifTrue: [self focusIndicator ~~ aMorph]
				ifFalse: [self focusIndicator key ~~ aMorph]])
		ifTrue: [self focusIndicator: aMorph ->(self newFocusIndicatorMorphFor: aMorph)].
	^self focusIndicator value
		privateBounds: aMorph focusBounds
]

{ #category : 'private' }
UITheme >> formSetsForCurrentDisplayScaleFactor [

	^ self formSetsForScale: self currentWorld displayScaleFactor
]

{ #category : 'private' }
UITheme >> formSetsForScale: scale [

	^ formSets at: scale ifAbsentPut: [ self newFormSetsForScale: scale ]
]

{ #category : 'accessing - colors' }
UITheme >> ghostTextColor [

	^ colorPalette ghostTextColor
]

{ #category : 'border-styles' }
UITheme >> groupLabelBorderStyleFor: aGroupPanel [
	"Answer the normal border style for a group label."

	^BorderStyle simple
		width: 0;
		baseColor: Color transparent
]

{ #category : 'border-styles' }
UITheme >> groupPanelBorderStyleFor: aGroupPanel [
	"Answer the normal border style for a group panel."

	^ SimpleBorderStyle new
		width: 1 scaledByDisplayScaleFactor;
		baseColor: ((self baseColorFor: aGroupPanel))
]

{ #category : 'border-styles' }
UITheme >> handlesWindowDropShadowInHandFor: aSystemWindow [
	"Answer whether the receiver will handle drop shadow drawing
	for a window when picked up in the hand. We will when diffuse."

	^self windowActiveDropShadowStyle == #diffuse and: [
		aSystemWindow isActive]
]

{ #category : 'basic-colors' }
UITheme >> highlightTextColor [
	"I am a color used to have a text color that is more visible than the classic text color to do some highlights."

	^ colorPalette highlightTextColor
]

{ #category : 'icons-utilities' }
UITheme >> icons [
	^ Smalltalk ui icons
]

{ #category : 'initialization' }
UITheme >> initialize [
	"Initialize the receiver."

	super initialize.
	colorPalette := UIThemePalette new
		colorConfigurator: self class colorConfiguratorClass new;
		yourself.
	formSets := Dictionary new.

	self initializeDefaultSettings.
	colorPalette themeSettings: settings.

	self windowActiveDropShadowStyle: #diffuse
]

{ #category : 'initialization' }
UITheme >> initializeDefaultSettings [

	settings := self defaultSettings
]

{ #category : 'accessing' }
UITheme >> labelAreaLayout [
	^ self settings labelAreaLayout
]

{ #category : 'label-styles' }
UITheme >> labelDisabledColorFor: aLabel [
	"^ aLabel owner ifNil: [ aLabel paneColor muchDarker ] ifNotNil: [
	aLabel owner color isTransparent ifTrue: [ aLabel owner paneColor muchDarker ]
	ifFalse: [ aLabel owner color muchDarker ] ]"

	"This method was overriden in the dark theme with the difference that the colors were
	converted using muchLighter"

	^ self disabledLabelColor
]

{ #category : 'defaults' }
UITheme >> labelFont [
	"Answer the label font to use."

	^self textFont
]

{ #category : 'accessing' }
UITheme >> lessConspicuousColorFrom: aColor [

	^ colorPalette lessConspicuousColorFrom: aColor
]

{ #category : 'accessing - colors' }
UITheme >> lightBackgroundColor [

	^ colorPalette lightBackgroundColor
]

{ #category : 'accessing - colors' }
UITheme >> lightBaseColor [

	^ colorPalette lightBaseColor
]

{ #category : 'private' }
UITheme >> lightColorFor: aButton [

	^ self lightBaseColor
]

{ #category : 'accessing' }
UITheme >> lightSelectionColor [

	^ colorPalette lightSelectionColor
]

{ #category : 'accessing - colors' }
UITheme >> lineNumberColor [

	^ colorPalette lineNumberColor
]

{ #category : 'accessing - colors' }
UITheme >> listBackgroundColor [

	^ colorPalette listBackgroundColor
]

{ #category : 'border-styles' }
UITheme >> listDisabledBorderStyleFor: aList [
	"Return the disabled borderStyle for the given list."

	^self listNormalBorderStyleFor: aList
]

{ #category : 'fill-styles' }
UITheme >> listDisabledFillStyleFor: aList [
	"Return the disabled fillStyle for the given list."

	^ self textEditorDisabledFillStyleFor: aList
]

{ #category : 'border-styles' }
UITheme >> listFocusBoundsFor: aListMorph [
	"Answer the bounds for drawing the focus indication."

	^aListMorph bounds
]

{ #category : 'defaults' }
UITheme >> listFont [
	"Answer the list font to use."

	^StandardFonts listFont
]

{ #category : 'border-styles' }
UITheme >> listNormalBorderStyleFor: aList [
	"Return the normal borderStyle for the given list"

	^BorderStyle simple
		width: 1 scaledByDisplayScaleFactor;
		baseColor: aList paneColor
]

{ #category : 'fill-styles' }
UITheme >> listNormalFillStyleFor: aList [

	"Return the normal fillStyle for the given list."

	^ colorPalette listNormalFillStyleFor: aList
]

{ #category : 'accessing - colors' }
UITheme >> listTextColor [

	^ colorPalette listTextColor
]

{ #category : 'services' }
UITheme >> longMessageIn: aThemedMorph text: aStringOrText title: aString [
	"Answer the result of a (potentially long) message dialog (true) with the given label and title."

	^ ((LongMessageDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText) openModal cancelled not
]

{ #category : 'defaults' }
UITheme >> menuBarFont [
	"Answer the menu bar font to use."

	^self textFont
]

{ #category : 'accessing - colors' }
UITheme >> menuBorderColor [

	^ colorPalette menuBorderColor
]

{ #category : 'accessing' }
UITheme >> menuBorderWidth [
	^  self settings menuBorderWidth
]

{ #category : 'label-styles' }
UITheme >> menuCloseForm [

	^self menuCloseFormSet asForm
]

{ #category : 'label-styles' }
UITheme >> menuCloseFormSet [
	"Answer the form set to use for the close button of a menu."

	^self windowCloseFormSet
]

{ #category : 'accessing - colors' }
UITheme >> menuColor [

	^ colorPalette menuColor
]

{ #category : 'fill-styles' }
UITheme >> menuColorFor: aThemedMorph [

	"Answer the menu color to use."

	| w |

	^ self menuColor
		ifNil: [ aThemedMorph ifNil: [ ^ self defaultMenuColor ].
			w := aThemedMorph isSystemWindow
				ifTrue: [ aThemedMorph ]
				ifFalse: [ aThemedMorph ownerThatIsA: SystemWindow ].
			w
				ifNil: [ self defaultMenuColor ]
				ifNotNil: [ ( w valueOfProperty: #paneColor ) ifNil: [ self defaultMenuColor ] ]
			]
]

{ #category : 'fill-styles' }
UITheme >> menuFillStyleFor: aMenu [
	"Return the fillStyle for the given menu."

	| bc fill |
	bc := aMenu valueOfProperty: #basicColor ifAbsent: [self settings derivedMenuColor].
	fill := GradientFillStyle ramp: {0.0 -> (bc alphaMixed: 0.2 with: Color white). 1.0 -> bc}.
	fill
		radial: false;
		origin: aMenu topLeft;
		direction: 0 @ aMenu height.
	^ fill
]

{ #category : 'defaults' }
UITheme >> menuFont [
	"Answer the menu font to use."

	^StandardFonts menuFont
]

{ #category : 'defaults' }
UITheme >> menuItemDisabledTextColorFor: aMenuItem [
	"Answer the color to use for disabled menu item text."

	^ colorPalette menuItemDisabledTextColorFor: aMenuItem
]

{ #category : 'border-styles - buttons' }
UITheme >> menuItemInDockingBarSelectedBorderStyleFor: aMenuItem [
	"Return the selected menu item borderStyle when in a docking bar for the given menu item."

	^self menuItemSelectedBorderStyleFor: aMenuItem
]

{ #category : 'fill-styles - buttons' }
UITheme >> menuItemInDockingBarSelectedFillStyleFor: aMenuItem [
	"Answer the selected fill style to use for the given menu item that is in a docking bar."
	^ self buttonSelectedFillStyleFor: aMenuItem
]

{ #category : 'defaults' }
UITheme >> menuItemNormalTextColorFor: aMenuItem [
	"Answer the color to use for normal menu item text."

	^((aMenuItem color luminance - aMenuItem owner paneColor luminance) abs < 0.3)
		ifTrue: [aMenuItem owner paneColor contrastingBlackAndWhiteColor]
		ifFalse: [aMenuItem color]
]

{ #category : 'border-styles - buttons' }
UITheme >> menuItemSelectedBorderStyleFor: aMenuItem [
	"Return the selected menu item borderStyle for the given menu item."

	^BorderStyle simple
		width: 0
]

{ #category : 'fill-styles - buttons' }
UITheme >> menuItemSelectedFillStyleFor: aMenuItem [
	"Answer the selected fill style to use for the given menu item."

	| fill baseColor preferenced |
	preferenced := self menuSelectionColor.
	baseColor := preferenced
		             ifNil: [ aMenuItem owner color negated ]
		             ifNotNil: [ preferenced ].
	self preferGradientFill ifFalse: [ ^ baseColor ].
	fill := GradientFillStyle ramp: {
			        (0.0 -> baseColor twiceLighter).
			        (1 -> baseColor twiceDarker) }.
	fill
		origin: aMenuItem topLeft;
		direction: 0 @ aMenuItem height.
	^ fill
]

{ #category : 'defaults' }
UITheme >> menuItemSelectedTextColorFor: aMenuItem [
	"Answer the color to use for selected menu item text."

	^ self settings menuSelectionTextColor
]

{ #category : 'accessing - colors' }
UITheme >> menuKeyboardFocusColor [

	^ colorPalette menuKeyboardFocusColor
]

{ #category : 'label-styles' }
UITheme >> menuPinForm [
	"Answer the form to use for the pin button of a menu."

	^ self iconNamed: #menuPinForm
]

{ #category : 'accessing - colors' }
UITheme >> menuSelectionColor [

	^ colorPalette menuSelectionColor
]

{ #category : 'accessing - colors' }
UITheme >> menuShadowColor [

	^ colorPalette menuShadowColor
]

{ #category : 'accessing - colors' }
UITheme >> menuTitleColor [

	^ colorPalette menuTitleColor
]

{ #category : 'fill-styles' }
UITheme >> menuTitleColorFor: aThemedMorph [

	"Answer the menu titlecolor to use."

	| w |

	^ self menuTitleColor
		ifNil: [ aThemedMorph ifNil: [ ^ self defaultMenuTitleColor ].
			w := aThemedMorph isSystemWindow
				ifTrue: [ aThemedMorph ]
				ifFalse: [ aThemedMorph ownerThatIsA: SystemWindow ].
			w
				ifNil: [ self defaultMenuTitleColor ]
				ifNotNil: [ ( w valueOfProperty: #paneColor ) darker ifNil: [ self defaultMenuTitleColor ] ]
			]
]

{ #category : 'fill-styles' }
UITheme >> menuTitleFillStyleFor: aMenuTitle [
	"Return the fillStyle for the given title menu."
	| fill |

	fill := GradientFillStyle ramp: {0.0 -> aMenuTitle color twiceLighter. 1 -> aMenuTitle color twiceDarker}.
	fill
		origin: aMenuTitle topLeft;
		direction: aMenuTitle width @ 0.
	^ fill
]

{ #category : 'border-styles - menubar' }
UITheme >> menubarBorderWidthFor: aMenubar [
	^ 0
]

{ #category : 'fill-styles - menubar' }
UITheme >> menubarFillStyleFor: aMenubar [
	^ self taskbarFillStyleFor: aMenubar
]

{ #category : 'fill-styles - menubar' }
UITheme >> menubarItemSelectionFillStyleFor: aMenubarItem [
	^ self menuSelectionColor
]

{ #category : 'fill-styles - menubar' }
UITheme >> menubarItemSelectionTextColorFor: aMenubarItem [
	^ self settings menuSelectionTextColor
]

{ #category : 'services' }
UITheme >> messageIn: aThemedMorph text: aStringOrText title: aString [
	"Answer the result of a message dialog (true) with the given label and title."

	^ ((MessageDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText) openModal cancelled not
]

{ #category : 'border-styles' }
UITheme >> morphTreeColumnBorderStyleFor: aMorph [
	^ self buttonNormalBorderStyleFor: aMorph
]

{ #category : 'fill-styles' }
UITheme >> morphTreeColumnFillStyleFor: aMorph [
	^ self buttonNormalFillStyleFor: aMorph
]

{ #category : 'fill-styles' }
UITheme >> morphTreeSplitterNormalFillStyleFor: aSplitter [
	^ self splitterNormalFillStyleFor: aSplitter
]

{ #category : 'fill-styles' }
UITheme >> morphTreeSplitterPressedFillStyleFor: aSplitter [
	"Return the pressed splitter fillStyle for the given splitter."

	|aColor|
	aColor := aSplitter paneColor twiceDarker.
	^(GradientFillStyle ramp: {
			0.0->Color white. 0.4-> aColor twiceLighter.
			1.0->aColor darker duller})
		origin: aSplitter topLeft;
		direction: (aSplitter splitsTopAndBottom
			ifTrue: [0 @ aSplitter height]
			ifFalse: [aSplitter width @ 0]);
		radial: false
]

{ #category : 'accessing - colors' }
UITheme >> mouseOverColor [
	"This color is used when hovering a raw of a list / table.
	This color should be different from the default background color and from the raw
	alternate color
	(see FTTableContainerMorph>>#alternateRowsColor."

	^ colorPalette mouseOverColor
]

{ #category : 'morph creation' }
UITheme >> newAlphaImageIn: aThemedMorph image: aForm help: helpText [
	"Answer an alpha image morph."

	|answer|
	answer := AlphaImageMorph new
		hResizing: #rigid;
		vResizing: #rigid;
		setBalloonText: helpText.
	aForm ifNotNil: [answer image: aForm].
	^answer
]

{ #category : 'morph creation' }
UITheme >> newAlphaSelectorIn: aThemedMorph for: aModel getAlpha: getSel setAlpha: setSel help: helpText [
	"Answer an alpha selector ."

	^(AColorSelectorMorph
			on: aModel
			getValue: getSel
			setValue: setSel)
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newAutoAcceptTextEntryIn: aThemedMorph for: aModel get: getSel set: setSel class: aClass getEnabled: enabledSel font: aFont help: helpText [
	"Answer a text entry for the given model.
	Object conversion will be automatic based on the class of
	object returned after performing the get selector, aClass
	is provided in case of initial nil returns."
	^ self
		newAutoAcceptTextEntryIn: aThemedMorph
		for: aModel
		get: getSel
		set: setSel
		class: aClass
		getEnabled: enabledSel
		font: aFont
		help: helpText
		entryCompletion: nil
]

{ #category : 'morph creation' }
UITheme >> newAutoAcceptTextEntryIn: aThemedMorph for: aModel  get: getSel set: setSel class: aClass getEnabled: enabledSel font: aFont help: helpText entryCompletion: anEntryCompletion [
	"Answer a text entry for the given model.
	Object conversion will be automatic based on the class of
	object returned after performing the get selector, aClass
	is provided in case of initial nil returns."

	|pt|
	pt := RubPluggableTextFieldMorph new
		autoAccept: true;
		alwaysAccept: true;
		convertTo: aClass;
		on: aModel
		text: getSel
		accept: setSel
		readSelection: nil
		menu: nil;
		acceptOnCR: true;
		getEnabledSelector: enabledSel;
		font: aFont;
		cornerStyle: (self textEntryCornerStyleIn: aThemedMorph);
		hResizing: #spaceFill;
		vResizing: #rigid;
		borderStyle: (BorderStyle inset width: 1 scaledByDisplayScaleFactor);
		setBalloonText: helpText;
		entryCompletion: anEntryCompletion.
	^pt
]

{ #category : 'morph creation' }
UITheme >> newAutoAcceptTextEntryIn: aThemedMorph for: aModel get: getSel set: setSel class: aClass getEnabled: enabledSel help: helpText [
	"Answer a text entry for the given model.
	Object conversion will be automatic based on the class of
	object returned after performing the get selector, aClass
	is provided in case of initial nil returns."
	^ self
		newAutoAcceptTextEntryIn: aThemedMorph
		for: aModel
		get: getSel
		set: setSel
		class: aClass
		getEnabled: enabledSel
		help: helpText
		entryCompletion: nil
]

{ #category : 'morph creation' }
UITheme >> newAutoAcceptTextEntryIn: aThemedMorph for: aModel  get: getSel set: setSel class: aClass getEnabled: enabledSel help: helpText entryCompletion: anEntryCompletion [
	"Answer a text entry for the given model.
	Object conversion will be automatic based on the class of
	object returned after performing the get selector, aClass
	is provided in case of initial nil returns."

	^self
		newAutoAcceptTextEntryIn: aThemedMorph
		for: aModel
		get: getSel
		set: setSel
		class: aClass
		getEnabled: enabledSel
		font: self textFont
		help: helpText
		entryCompletion: anEntryCompletion
]

{ #category : 'morph creation' }
UITheme >> newBalloonHelpIn: aThemedMorph contents: aTextStringOrMorph for: aMorph corner: cornerSymbol [
	"Answer a new balloon help morph with the given text
	and positioning for aMorph."

	^SimpleBalloonMorph
		string: aTextStringOrMorph
		for: aMorph
		corner: cornerSymbol
]

{ #category : 'morph creation' }
UITheme >> newBracketSliderIn: aThemedMorph for: aModel getValue: getSel setValue: setSel min: minValue max: maxValue quantum: quantum getEnabled: enabledSel help: helpText [
	"Answer a bracket slider."

	^(BracketSliderMorph
			on: aModel
			getValue: getSel
			setValue: setSel
			min: minValue
			max: maxValue
			quantum: quantum)
		getEnabledSelector: enabledSel;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newButtonIn: aThemedMorph for: aModel getState: stateSel action: actionSel arguments: args getEnabled: enabledSel getLabel: labelSel help: helpText [
	"Answer a new button."

	|b|
	b := PluggableButtonMorph
			on: aModel
			getState: stateSel
			action: actionSel
			label: labelSel.
	b
		theme: self;
		label: ' ' font: self buttonFont;
		update: labelSel;
		arguments: (args ifNil: [{b}]);
		getEnabledSelector: enabledSel;
		cornerStyle: (self buttonCornerStyleIn: aThemedMorph);
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: helpText;
		extent: b minExtent;
		removeProperty: #theme.
	^b
]

{ #category : 'morph creation' }
UITheme >> newButtonIn: aThemedMorph for: aModel getState: stateSel action: actionSel arguments: args getEnabled: enabledSel label: label help: helpText [
	"Answer a new button."

	| button |
	^ (button := PluggableButtonMorph on: aModel getState: stateSel action: actionSel)
		theme: self;
		arguments: (args ifNil: [ {button} ]);
		cornerStyle: (self buttonCornerStyleIn: aThemedMorph);
		label: label font: self buttonFont;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText;
		extent: button minExtent;
		hResizing: #rigid;
		vResizing: #rigid;
		removeProperty: #theme;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newButtonLabelIn: aThemedMorph for: aModel label: aString getEnabled: enabledSel [
	"Answer a new button text label."

	^(self buttonLabelForText: aString)
		model: aModel;
		getEnabledSelector: enabledSel
]

{ #category : 'morph creation' }
UITheme >> newCancelButtonIn: aThemedMorph for: aModel [
	"Answer a new cancel button."

	^self
		newButtonIn: aThemedMorph
		for: aModel
		getState: nil
		action: #cancel
		arguments: nil
		getEnabled: nil
		label: 'Cancel' translated
		help: 'Cancel changes and close the window' translated
]

{ #category : 'morph creation' }
UITheme >> newCheckboxIn: aThemedMorph for: aModel getSelected: getSel setSelected: setSel getEnabled: enabledSel getLabel: labelSel help: helpText [
	"Answer a checkbox with the given label ."

	| answer |
	answer := CheckboxMorph
			on: aModel selected: getSel changeSelected: setSel.
	answer
		getEnabledSelector: enabledSel;
		getLabelSelector: labelSel;
		labelMorph: (self checkboxLabelFor: answer);
		font: self labelFont;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: helpText.
	^answer
]

{ #category : 'morph creation' }
UITheme >> newCheckboxIn: aThemedMorph for: aModel getSelected: getSel setSelected: setSel getEnabled: enabledSel label: label help: helpText [
	"Answer a checkbox with the given label ."

	| answer |
	answer := CheckboxMorph
			on: aModel selected: getSel changeSelected: setSel.
	answer
		getEnabledSelector: enabledSel;
		label: label;
		labelMorph: (self checkboxLabelFor: answer);
		font: self labelFont;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: helpText.
	^answer
]

{ #category : 'morph creation' }
UITheme >> newCloseButtonIn: aThemedMorph for: aModel [
	"Answer a new close button."

	^self
		newButtonIn: aThemedMorph
		for: aModel
		getState: nil
		action: #close
		arguments: nil
		getEnabled: nil
		label: 'Close' translated
		help: 'Close the window' translated
]

{ #category : 'morph creation' }
UITheme >> newCloseControlIn: aThemedMorph for: aModel action: aValuable help: helpText [
	"Answer a button for closing things."

	|formSet msb|
	formSet := self windowCloseFormSet.
	msb := MultistateButtonMorph new extent: formSet extent.
	msb activeEnabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowClosePassiveFormSet.
	msb extent: formSet extent.
	msb activeDisabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	msb passiveEnabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	msb passiveDisabledNotOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowCloseOverFormSet.
	msb extent: formSet extent.
	msb
		activeEnabledOverUpFillStyle: (ImageFillStyle formSet: formSet);
		passiveEnabledOverUpFillStyle: (ImageFillStyle formSet: formSet).
	formSet := self windowClosePassiveFormSet.
	msb
		extent: formSet extent;
		activeEnabledOverDownFillStyle: (ImageFillStyle formSet: formSet);
		passiveEnabledOverDownFillStyle: (ImageFillStyle formSet: formSet);
		addUpAction: aValuable;
		setBalloonText: helpText.
	^msb
]

{ #category : 'morph creation' }
UITheme >> newColorChooserIn: aThemedMorph for: aModel getColor: getSel setColor: setSel getEnabled: enabledSel help: helpText [
	"Answer a color chooser ."

	^(ColorChooserMorph
			on: aModel color: getSel changeColor: setSel)
		getEnabledSelector: enabledSel;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newColorPresenterIn: aThemedMorph for: aModel getColor: getSel help: helpText [
	"Answer a color presenter."

	^(ColorPresenterMorph
			on: aModel color: getSel)
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newColumnIn: aThemedMorph for: controls [
	"Answer a morph laid out with a column of controls."

	|answer|
	answer := PanelMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		fillStyle: Color transparent; "non pane color tracking"
		changeTableLayout;
		cellInset: 8 scaledByDisplayScaleFactor;
		cellPositioning: #leftCenter.
	controls do: [:m | answer addMorphBack: m].
	^answer
]

{ #category : 'morph creation' }
UITheme >> newDialogPanelIn: aThemedMorph [
	"Answer a new (main) dialog panel."

	^(self newPanelIn: aThemedMorph)
		fillStyle: (SolidFillStyle color: Color transparent) "no pane colour tracking"
]

{ #category : 'morph creation' }
UITheme >> newDropListIn: aThemedMorph for: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel useIndex: useIndex help: helpText [
	"Answer a drop list for the given model."

	^(EditableDropListMorph basicNew
		basicTheme: self;
			initialize;
			useSelectionIndex: useIndex;
			on: aModel
			list: listSel
			selected: getSel
			changeSelected: setSel)
		selectionColor: self selectionColor;
		getEnabledSelector: enabledSel;
		font: self dropListFont;
		listFont: self listFont;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText;
		basicTheme: nil;
		beReadOnly;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newEditableDropListIn: aThemedMorph for: aModel list: listSel getSelected: getSel setSelected: setSel addToList: addSel class: aClass default: defaultValue ghostText: ghostText getEnabled: enabledSel useIndex: useIndex help: helpText [
	"Answer an editable drop list for the given model."

	^(EditableDropListMorph
			on: aModel
			list: listSel
			selected: getSel
			changeSelected: setSel
			useIndex: useIndex
			addToList: addSel
			class: aClass
			getEnabled: enabledSel
			default: defaultValue)
		ghostText: ghostText;
		selectionColor: self selectionColor;
		font: self dropListFont;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newEmbeddedMenuIn: aThemedMorph for: aModel [
	"Answer a new menu."

	^EmbeddedMenuMorph new
		defaultTarget: aModel;
		color: (self menuColorFor: aThemedMorph)
]

{ #category : 'morph creation' }
UITheme >> newExpanderIn: aThemedMorph label: aString forAll: controls [
	"Answer an expander with the given label and controls."

	|answer|
	answer := ExpanderMorph basicNew
		basicTheme: self;
		initialize;
		font: self menuFont;
		titleText: aString;
		basicTheme: nil.
	controls do: [:m |
		answer addMorphBack: m].
	^answer
]

{ #category : 'morph creation' }
UITheme >> newFocusIndicatorMorphFor: aMorph [
	"Answer a new focus indicator for the given morph."

	^ BorderedMorph new
		fillStyle: Color transparent;
		borderStyle: (SimpleBorderStyle new
			 width: 1 scaledByDisplayScaleFactor;
			baseColor: (self baseSelectionColorFor: aMorph));
		bounds: aMorph focusBounds
]

{ #category : 'private' }
UITheme >> newFormSetsForScale: scale [

	| size color |

	size := (9 * scale) truncated.
	color := Color gray: 0.5327468230694037.
	^ Dictionary new
		at: #treeExpanded put: (self newTreeFormSet: true size: size color: color);
		at: #treeUnexpanded put: (self newTreeFormSet: false size: size color: color);
		at: #whiteTreeExpanded put: (self newTreeFormSet: true size: size color: Color white);
		at: #whiteTreeUnexpanded put: (self newTreeFormSet: false size: size color: Color white);
		yourself
]

{ #category : 'morph creation' }
UITheme >> newFuzzyLabelIn: aThemedMorph for: aModel label: aString offset: offset alpha: alpha getEnabled: enabledSel [
	"Answer a new fuzzy label."

	^(FuzzyLabelMorph contents: aString font: self labelFont)
		offset: offset;
		alpha: alpha;
		model: aModel;
		getEnabledSelector: enabledSel
]

{ #category : 'morph creation' }
UITheme >> newGroupboxIn: aThemedMorph [
	"Answer a plain groupbox."

	^PlainGroupboxMorph new
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newGroupboxIn: aThemedMorph for: control [
	"Answer a plain groupbox."

	^(self newGroupboxIn: aThemedMorph)
		addMorph: control
]

{ #category : 'morph creation' }
UITheme >> newGroupboxIn: aThemedMorph forAll: controls [
	"Answer a plain groupbox."

	^self
		newGroupboxIn: aThemedMorph
		for: (self newColumnIn: aThemedMorph for: controls)
]

{ #category : 'morph creation' }
UITheme >> newGroupboxIn: aThemedMorph label: aString [
	"Answer a groupbox with the given label."

	^GroupboxMorph new
		font: self labelFont;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		label: aString;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newGroupboxIn: aThemedMorph label: aString for: control [
	"Answer a groupbox with the given label and control."

	^(self newGroupboxIn: aThemedMorph label: aString)
		addContentMorph: control;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newGroupboxIn: aThemedMorph label: aString forAll: controls [
	"Answer a groupbox with the given label and controls."

	^(self newGroupboxIn: aThemedMorph label: aString)
		addContentMorph: (self newColumnIn: aThemedMorph for: controls);
		yourself
]

{ #category : 'morph creation' }
UITheme >> newHSVASelectorIn: aThemedMorph color: aColor help: helpText [
	"Answer a hue-saturation-volume-alpha selector."

	^HSVAColorSelectorMorph new
		selectedColor: aColor;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newHSVSelectorIn: aThemedMorph color: aColor help: helpText [
	"Answer a hue-saturation-volume selector."

	^HSVColorSelectorMorph new
		selectedColor: aColor;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newHueSelectorIn: aThemedMorph for: aModel getHue: getSel setHue: setSel help: helpText [
	"Answer a hue selector ."

	^(HColorSelectorMorph
			on: aModel
			getValue: getSel
			setValue: setSel)
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newImageIn: aThemedMorph for: aModel get: getSel help: helpText [
	"Answer a new image morph."

	^AlphaImageMorph new
		model: aModel;
		borderStyle: (BorderStyle inset width: 0);
		getImageSelector: getSel;
		setBalloonText: helpText;
		updateImage
]

{ #category : 'morph creation' }
UITheme >> newImageIn: aThemedMorph form: aForm [
	"Answer a new text label."

	^self
		newImageIn: aThemedMorph
		form: aForm
		size: aForm extent
]

{ #category : 'morph creation' }
UITheme >> newImageIn: aThemedMorph form: aForm size: aPoint [
	"Answer a new image morph."

	^AlphaImageMorph new
		borderStyle: (BorderStyle inset width: 0);
		image: aForm size: aPoint
]

{ #category : 'morph creation' }
UITheme >> newIncrementalSliderIn: aThemedMorph for: aModel getValue: getSel setValue: setSel min: min max: max quantum: quantum getEnabled: enabledSel help: helpText [
	"Answer a slider inrement/decrement buttons."

	^(IncrementalSliderMorph
			on: aModel
			getValue: getSel
			setValue: setSel
			min: min
			max: max
			quantum: quantum)
		getEnabledSelector: enabledSel;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newLabelGroupIn: aThemedMorph for: labelsAndControls [
	"Answer a morph laid out with a column of labels and a column of associated controls.
	The vResizing for each row will be #spaceFill if the control for that row specifies
	this, otherwise #shrinkWrap."

	| labels labelWidth lc |
	lc := labelsAndControls collect: [:a |
		(a key isMorph
			ifTrue: [a key]
			ifFalse: [a key ifNotNil: [self newLabelIn: aThemedMorph label: a key asString]])
			-> a value].
	labels := Morph new
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		changeTableLayout.
	lc do: [:a |
		a key ifNotNil: [labels addMorphBack: a key]].
	labelWidth := labels minExtent x.
	^self newColumnIn: aThemedMorph for: (lc collect: [:a | | row |
		a key ifNotNil: [a key hResizing: #rigid; extent: labelWidth@ a key height].
		row := self newRowIn: aThemedMorph for: (a key ifNil: [{a value}] ifNotNil: [{a key. a value}]).
		row vResizing: (a value vResizing = #spaceFill ifTrue: [#spaceFill] ifFalse: [#shrinkWrap]).
		row])
]

{ #category : 'morph creation' }
UITheme >> newLabelGroupIn: aThemedMorph for: labelsAndControls font: aFont labelColor: aColor [
	"Answer a morph laid out with a column of labels and a column of associated controls.
	If spaceFill is tru then each row will share available space to pad."

	| labels labelWidth lc|
	lc := labelsAndControls collect: [:a |
		(a key isMorph
			ifTrue: [a key]
			ifFalse: [(self newLabelIn: aThemedMorph label: (a key ifNil: ['']) asString)
						font: aFont;
						color: aColor])
			-> a value].
	labels := Morph new
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		changeTableLayout.
	lc do: [:a |
		labels addMorphBack: a key].
	labelWidth := labels minExtent x.
	^self newColumnIn: aThemedMorph for: (lc collect: [:a | | row |
		a key hResizing: #rigid; extent: labelWidth@ a key height.
		row := self newRowIn: aThemedMorph for: {a key. a value}.
		row vResizing: (a value vResizing = #spaceFill ifTrue: [#spaceFill] ifFalse: [#shrinkWrap]).
		row])
]

{ #category : 'morph creation' }
UITheme >> newLabelIn: aThemedMorph for: aModel getLabel: labelSel getEnabled: enabledSel [
	"Answer a new text label."

	^(LabelMorph contents: '' font: self labelFont)
		model: aModel;
		getEnabledSelector: enabledSel;
		getTextSelector: labelSel
]

{ #category : 'morph creation' }
UITheme >> newLabelIn: aThemedMorph for: aModel label: aString getEnabled: enabledSel [
	"Answer a new text label."

	^(LabelMorph contents: aString font: self labelFont)
		model: aModel;
		getEnabledSelector: enabledSel
]

{ #category : 'morph creation' }
UITheme >> newLabelIn: aThemedMorph label: aString [
	"Answer a new text label."

	^self
		newLabelIn: aThemedMorph
		for: nil
		label: aString
		getEnabled: nil
]

{ #category : 'morph creation' }
UITheme >> newListIn: aThemedMorph for: aModel list: listSelector selected: getSelector changeSelected: setSelector getEnabled: enabledSel help: helpText [
	"Answer a list for the given model."

	^PluggableListMorph new
		selectionColor: self selectionColor;
		textColor: self listTextColor;
		font: self listFont;
		on: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		menu: nil
		keystroke: nil;
		autoDeselect: false;
		cornerStyle: aThemedMorph preferredCornerStyle;
		color: Color white;
		borderStyle: (BorderStyle inset width: 1 scaledByDisplayScaleFactor);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newListIn: aThemedMorph for: aModel list: listSelector selected: getSelector changeSelected: setSelector icon: iconSelector getEnabled: enabledSel help: helpText [
	"Answer an icon list for the given model."

	^PluggableIconListMorph new
		selectionColor: self selectionColor;
		textColor: self listTextColor;
		font: self listFont;
		on: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		menu: nil
		keystroke: nil;
		autoDeselect: false;
		cornerStyle: aThemedMorph preferredCornerStyle;
		color: Color white;
		borderStyle: (BorderStyle inset width: 1 scaledByDisplayScaleFactor);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		getIconSelector: iconSelector;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newMenuIn: aThemedMorph for: aModel [
	"Answer a new menu."

	^MenuMorph new
		defaultTarget: aModel;
		color: (self menuColorFor: aThemedMorph)
]

{ #category : 'morph creation' }
UITheme >> newMorphDropListIn: aThemedMorph for: aModel list: listSel getSelected: getSel setSelected: setSel getEnabled: enabledSel useIndex: useIndex help: helpText [
	"Answer a morph drop list for the given model."

	^(MorphDropListMorph
			on: aModel
			list: listSel
			selected: getSel
			changeSelected: setSel
			useIndex: useIndex)
		selectionColor: self selectionColor;
		getEnabledSelector: enabledSel;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newMorphListIn: aThemedMorph for: aModel list: listSelector getSelected: getSelector setSelected: setSelector getEnabled: enabledSel help: helpText [
	"Answer a morph list for the given model."

	^(PluggableMorphListMorph
			on: aModel
			list: listSelector
			selected: getSelector
			changeSelected: setSelector
			menu: nil
			keystroke: nil)
		selectionColor: self selectionColor;
		autoDeselect: false;
		cornerStyle: aThemedMorph preferredCornerStyle;
		color: Color white;
		borderStyle: (BorderStyle inset width: 1 scaledByDisplayScaleFactor);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newMultistateButtonIn: aThemedMorph [
	"Answer a new multistate button morph."

	^MultistateButtonMorph new
]

{ #category : 'morph creation' }
UITheme >> newNoButtonIn: aThemedMorph for: aModel [
	"Answer a new No button."

	^(self
			newButtonIn: aThemedMorph
			for: aModel
			getState: nil
			action: #no
			arguments: nil
			getEnabled: nil
			label: 'No' translated
			help: 'Answer no and close the window' translated)
		hResizing: #rigid;
		vResizing: #rigid
]

{ #category : 'morph creation' }
UITheme >> newOKButtonIn: aThemedMorph for: aModel getEnabled: enabledSel [
	"Answer a new OK button."

	^self
		newButtonIn: aThemedMorph
		for: aModel
		getState: nil
		action: #ok
		arguments: nil
		getEnabled: enabledSel
		label: 'OK' translated
		help: 'Apply changes and close the window' translated
]

{ #category : 'morph creation' }
UITheme >> newOverflowRowIn: aThemedMorph forAll: aCollectionOfMorphs [
	"Answer a new overflow row morph with the given contents."

	^OverflowRowMorph new
		baseMorphs: aCollectionOfMorphs;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newPanelIn: aThemedMorph [
	"Answer a new panel."

	^PanelMorph new
		changeTableLayout;
		layoutInset: 4 scaledByDisplayScaleFactor;
		cellInset: 8 scaledByDisplayScaleFactor;
		cornerStyle: aThemedMorph preferredCornerStyle;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newPluggableDialogWindowIn: aThemedMorph title: title for: contentMorph [
	"Answer a new pluggable dialog panel with the given content."

	|answer|
	answer := PluggableDialogWindow new
		setWindowColor: aThemedMorph paneColor;
		title: title;
		contentMorph: contentMorph.
	contentMorph ifNotNil: [answer model: nil].
	^answer
]

{ #category : 'morph creation' }
UITheme >> newRadioButtonIn: aThemedMorph for: aModel getSelected: getSel setSelected: setSel getEnabled: enabledSel label: label help: helpText [
	"Answer a checkbox (radio button appearance) with the given label ."

	| answer |
	answer := CheckboxMorph
			on: aModel selected: getSel changeSelected: setSel.
	answer
		getEnabledSelector: enabledSel;
		label: label;
		labelMorph: (self checkboxLabelFor: answer);
		font: self labelFont;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: helpText;
		beRadioButton.
	^answer
]

{ #category : 'morph creation' }
UITheme >> newRowIn: aThemedMorph for: controls [
	"Answer a morph laid out with a row of controls."

	|answer|
	answer := PanelMorph new
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		fillStyle: Color transparent; "non pane color tracking"
		changeTableLayout;
		listDirection: #leftToRight;
		cellInset: 8 scaledByDisplayScaleFactor;
		cellPositioning: #center.
	controls do: [:m | answer addMorphBack: m].
	^answer
]

{ #category : 'morph creation' }
UITheme >> newSVSelectorIn: aThemedMorph color: aColor help: helpText [
	"Answer a saturation-volume selector."

	^SVColorSelectorMorph new
		color: aColor;
		selectedColor: aColor;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		setBalloonText: helpText
]

{ #category : 'morph creation' }
UITheme >> newScrollBarDownButtonFor: aScrollbar [
	^ BorderedMorph newBounds: (self scrollBarDownButtonBoundsFor: aScrollbar)
]

{ #category : 'morph creation' }
UITheme >> newScrollBarUpButtonFor: aScrollbar [
	^ BorderedMorph newBounds: (self scrollBarUpButtonBoundsFor: aScrollbar)
]

{ #category : 'morph creation' }
UITheme >> newScrollPaneIn: aThemedMorph for: aMorph [
	"Answer a new GeneralScrollPane with given content morph."

	^GeneralScrollPaneMorph new
		scrollTarget: aMorph;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newSeparatorIn: aThemedMorph [
	"Answer a new horizontal separator."

	^SeparatorMorph new
		fillStyle: Color transparent;
		borderStyle: (BorderStyle inset baseColor: Color blue; width: 1 scaledByDisplayScaleFactor);
		extent: (2@2) scaledByDisplayScaleFactor;
		hResizing: #spaceFill
]

{ #category : 'morph creation' }
UITheme >> newSliderIn: aThemedMorph for: aModel getValue: getSel setValue: setSel min: min max: max quantum: quantum getEnabled: enabledSel help: helpText [
	"Answer a slider. Resizing set for horizontal usage by default."

	^(PluggableSliderMorph
			on: aModel
			getValue: getSel
			setValue: setSel
			min: min
			max: max
			quantum: quantum)
		getEnabledSelector: enabledSel;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		setBalloonText: helpText;
		minHeight: 16 scaledByDisplayScaleFactor
]

{ #category : 'morph creation' }
UITheme >> newStackIn: aThemedMorph for: controls [
	"Answer a morph laid out with a stack of controls."

	|answer|
	answer := PanelMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		fillStyle: Color transparent; "non pane color tracking"
		layoutPolicy: StackLayout new.
	controls do: [:m | answer addMorphBack: m].
	^answer
]

{ #category : 'morph creation' }
UITheme >> newStringIn: aThemedMorph label: aStringOrText font: aFont style: aStyle [
	"Answer a new string/text morph."

	^(EmbossedStringMorph contents: aStringOrText font: aFont)
		style: aStyle
]

{ #category : 'morph creation' }
UITheme >> newTabGroupIn: aThemedMorph for: labelsAndControls [
	"Answer a tab group morph with the given tab labels and associated pages."

	^(TabGroupMorph new
		font: self labelFont;
		cornerStyle: (self tabGroupCornerStyleIn: aThemedMorph);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		labelsAndPages: labelsAndControls)
		selectedPageIndex: (labelsAndControls isEmpty ifTrue: [0] ifFalse: [1])
]

{ #category : 'morph creation' }
UITheme >> newTaskListButtonIn: aTasklist for: aTask [
	"Answer a tasklist button morph for the given task."

	|lm lab button |
	lab := (self buttonLabelForText: (aTask label contractTo: 40)).
	aTask isActive
		ifTrue: [ lab emphasis: 1 "1 -> bold" ].
	lm := self
		newRowIn: aTasklist
		for: {(aTask icon ifNil: [self iconNamed: #smallWindow]) asMorph. lab}.
	button := self
		newButtonIn: aTasklist
		for: aTask morph
		getState: #isActive
		action: #buttonClickedForTaskList:
		arguments: {aTasklist}
		getEnabled: nil
		label: lm
		help: nil.
	button
		useSquareCorners;
		onColor: (self taskbarMinimizedButtonColorFor: button)
		offColor: (aTask isActive
				ifTrue: [self taskbarActiveButtonColorFor: button]
				ifFalse: [self taskbarButtonColorFor: button]);
		hResizing: #spaceFill.
	lab color: (self taskbarButtonLabelColorFor: button).
	button model: aTask.
	^button
]

{ #category : 'morph creation' }
UITheme >> newTaskbarButtonIn: aTaskbar for: aWindow [
	"Answer a taskbar button morph for the given window."

	^ (TaskbarItemMorph on: aWindow getState: #isActive action: #taskbarButtonClicked)
		initializeFor: aTaskbar;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newTaskbarThumbnailIn: aThemedMorph for: aWindow [
	"Answer a taskbar thumbnail morph for the given window."

	|answer thumb|
	thumb := aWindow taskbarThumbnail.
	answer := PanelMorph new
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		changeTableLayout;
		layoutInset: 8 scaledByDisplayScaleFactor;
		cellInset: 4 scaledByDisplayScaleFactor;
		addMorphBack: thumb;
		addMorphBack: ((self
			buttonLabelForText: (aWindow labelString truncateWithElipsisTo: 50))
				color: Color white).
	answer
		extent: answer minExtent;
		fillStyle: (self taskListFillStyleFor: answer);
		borderStyle: (self taskbarThumbnailNormalBorderStyleFor: aWindow);
		cornerStyle: (self taskbarThumbnailCornerStyleFor: answer).
	^answer
]

{ #category : 'morph creation' }
UITheme >> newTextEditorIn: aThemedMorph for: aModel getText: getSel setText: setSel getEnabled: enabledSel menu: aMenuSelector [
	"Answer a text (Smalltalk code) editor for the given model."

	^ RubPluggableTextMorph new
		on: aModel
		text: getSel
		accept: setSel
		readSelection: nil
		menu: aMenuSelector;
		theme: self;
		getEnabledSelector: enabledSel;
		font: self textFont;
		cornerStyle: aThemedMorph preferredCornerStyle;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderStyle: (BorderStyle inset width: 1 scaledByDisplayScaleFactor);
		removeProperty: #theme
]

{ #category : 'morph creation' }
UITheme >> newTextEntryIn: aThemedMorph for: aModel get: getSel set: setSel class: aClass getEnabled: enabledSel help: helpText [
	"Answer a text entry for the given model."

	|pt|
	pt := RubPluggableTextFieldMorph new
		convertTo: aClass;
		alwaysAccept: true;
		on: aModel
		text: getSel
		accept: setSel
		readSelection: nil
		menu: nil;
		acceptOnCR: true;
		getEnabledSelector: enabledSel;
		font: self textFont;
		cornerStyle: (self textEntryCornerStyleIn: aThemedMorph);
		hResizing: #spaceFill;
		vResizing: #rigid;
		setBalloonText: helpText.
	^pt
]

{ #category : 'morph creation' }
UITheme >> newTextEntryIn: aThemedMorph for: aModel get: getSel set: setSel class: aClass getEnabled: enabledSel help: helpText entryCompletion: anEntryCompletion [
	"Answer a text entry for the given model."

	|pt|
	pt := RubPluggableTextFieldMorph new
		convertTo: aClass;
		alwaysAccept: true;
		on: aModel
		text: getSel
		accept: setSel
		readSelection: nil
		menu: nil;
		acceptOnCR: true;
		getEnabledSelector: enabledSel;
		font: self textFont;
		cornerStyle: (self textEntryCornerStyleIn: aThemedMorph);
		hResizing: #spaceFill;
		vResizing: #rigid;
		setBalloonText: helpText;
		entryCompletion: anEntryCompletion.
	^pt
]

{ #category : 'morph creation' }
UITheme >> newTextIn: aThemedMorph text: aStringOrText [
	"Answer a new text."

	^TextMorph new
		wrapFlag: true;
		contents: aStringOrText;
		font: self textFont;
		autoFit: true;
		lock;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap
]

{ #category : 'morph creation' }
UITheme >> newTitleIn: aThemedMorph label: aString for: control [
	"Answer a morph laid out with a column with the title and control aligned to the left."

	^(self newColumnIn: aThemedMorph for: {
		self newLabelIn: aThemedMorph label: aString.
		control})
		cellPositioning: #leftCenter;
		cellInset: 2 scaledByDisplayScaleFactor
]

{ #category : 'morph creation' }
UITheme >> newToolDockingBarIn: aThemedMorph [
	"Answer a new tool docking bar."

	^ToolDockingBarMorph new
		borderWidth: 0;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		clipSubmorphs: true
]

{ #category : 'morph creation' }
UITheme >> newToolSpacerIn: aThemedMorph [
	"Answer a new tool spacer."

	^Morph new
		borderWidth: 0;
		color: Color transparent;
		extent: (3@3) scaledByDisplayScaleFactor
]

{ #category : 'morph creation' }
UITheme >> newToolbarHandleIn: aThemedMorph [
	"Answer a new toolbar handle."

	^PanelMorph new
		fillStyle: Color transparent; "non pane color tracking"
		borderStyle: (BorderStyle raised baseColor: Color blue; width: 1 scaledByDisplayScaleFactor);
		extent: (4@3) scaledByDisplayScaleFactor;
		vResizing: #spaceFill
]

{ #category : 'morph creation' }
UITheme >> newToolbarIn: aThemedMorph [
	"Answer a new toolbar."

	|bar|
	bar := PanelMorph new
		changeTableLayout;
		layoutInset: (0@1) scaledByDisplayScaleFactor;
		listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	"bar
		addMorphBack: (self newToolSpacerIn: aThemedMorph);
		addMorphBack: (self newToolbarHandleIn: aThemedMorph);
		addMorphBack: (self newToolSpacerIn: aThemedMorph)."
	^bar
]

{ #category : 'morph creation' }
UITheme >> newToolbarIn: aThemedMorph for: controls [
	"Answer a new toolbar with the given controls."

	|answer|
	answer := self newToolbarIn: aThemedMorph.
	controls do: [:m | answer addMorphBack: m].
	^answer
]

{ #category : 'private' }
UITheme >> newTreeForm: expanded size: size color: color [

	| rectangle insetDivisor triangleRectangle rectangleScaled triangleRectangleScaled vertices formCanvas |

	rectangle := 0 asPoint extent: size asPoint.
	insetDivisor := 13.
	triangleRectangle := rectangle insetBy: rectangle extent / insetDivisor.
	rectangleScaled := rectangle scaleBy: insetDivisor.
	triangleRectangleScaled := triangleRectangle scaleBy: insetDivisor.
	vertices := expanded
		ifTrue: [ { triangleRectangleScaled topLeft. triangleRectangleScaled bottomCenter. triangleRectangleScaled topRight } ]
		ifFalse: [ { triangleRectangleScaled topLeft. triangleRectangleScaled rightCenter. triangleRectangleScaled bottomLeft } ].
	formCanvas := FormCanvas extent: rectangleScaled extent.
	formCanvas drawPolygon: vertices fillStyle: color.
	^ formCanvas form magnifyBy: insetDivisor reciprocal smoothing: insetDivisor
]

{ #category : 'private' }
UITheme >> newTreeFormSet: expanded size: size color: color [

	^ FormSet forms: {
		self newTreeForm: expanded size: size color: color.
		self newTreeForm: expanded size: size * 2 color: color }
]

{ #category : 'morph creation' }
UITheme >> newTreeIn: aThemedMorph for: aModel list: listSelector selected: getSelector changeSelected: setSelector [
	"Answer a new tree morph."

	^TreeListMorph new
		selectionColor: self selectionColor;
		font: self listFont;
		on: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector
		menu: nil
		keystroke: nil;
		cornerStyle: aThemedMorph preferredCornerStyle;
		"color: Color white;"
		borderStyle: (BorderStyle inset width: 1 scaledByDisplayScaleFactor);
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		autoDeselect: false;
		yourself
]

{ #category : 'morph creation' }
UITheme >> newVerticalSeparatorIn: aThemedMorph [
	"Answer a new vertical separator."

	^SeparatorMorph new
		fillStyle: Color transparent;
		borderStyle: (BorderStyle inset baseColor: Color blue; width: 1 scaledByDisplayScaleFactor);
		extent: (2@2) scaledByDisplayScaleFactor;
		vResizing: #spaceFill
]

{ #category : 'morph creation' }
UITheme >> newWindowHeaderFor: aWindow [

	^ (AlignmentMorph newSpacer: Color transparent)
			vResizing: #spaceFill;
			layoutPolicy: ProportionalLayout new
]

{ #category : 'morph creation' }
UITheme >> newWindowIn: aThemedMorph for: aModel title: titleString [
	"Answer a new StandardWindow with given model and title."

	^StandardWindow new
		title: titleString;
		model: aModel;
		yourself
]

{ #category : 'initialization' }
UITheme >> newWindowMenuPassiveForm [

	^ self newWindowMenuPassiveFormSet asForm
]

{ #category : 'initialization' }
UITheme >> newWindowMenuPassiveFormSet [
	"Answer a new form set for a window menu box."

	^ self iconFormSetNamed: #windowMenuInactive
]

{ #category : 'morph creation' }
UITheme >> newYesButtonIn: aThemedMorph for: aModel [
	"Answer a new Yes button."

	^(self
			newButtonIn: aThemedMorph
			for: aModel
			getState: nil
			action: #yes
			arguments: nil
			getEnabled: nil
			label: 'Yes' translated
			help: 'Answer yes and close the window' translated)
		hResizing: #rigid;
		vResizing: #rigid
]

{ #category : 'private' }
UITheme >> normalFillStyleFor: aMorph height: anInteger [
	"Return the normal button fillStyle for the given button."

	^ SolidFillStyle color: (self lightColorFor: aMorph)
]

{ #category : 'accessing' }
UITheme >> numberOfStepsForAnimations [
	^ self animationSettings numberOfSteps
]

{ #category : 'cursor' }
UITheme >> overTextCursorFor: aTextMorph [
	"Answer the pointer cursor when over an active text
	(answer nil if no particular cursor is wanted)."

	^ Cursor overEditableText
]

{ #category : 'fill-styles' }
UITheme >> paneColorFor: aWindow [
	"Answer the pane color that should be used."

	^ (self fadedBackgroundWindows
		   ifTrue: [ aWindow fillStyleToUse ]
		   ifFalse: [ self windowActiveFillStyle ]) asColor
]

{ #category : 'services' }
UITheme >> passwordEntryIn: aThemedMorph text: aStringOrText title: aString entryText: defaultEntryText [
	"Answer the result of a password entry dialog (a string or nil if cancelled)
	with the given label and title."

	^((PasswordDialogWindow newWithTheme: aThemedMorph theme)
			textFont: self textFont;
			title: aString;
			text: aStringOrText;
			entryText: defaultEntryText) openModal entryText
]

{ #category : 'basic-colors' }
UITheme >> patchworkUIThemeColorFor: aWindowOrModel [

	"Answer the colour for the given window."

	^ colorPalette patchworkUIThemeColorFor: aWindowOrModel
]

{ #category : 'border-styles' }
UITheme >> plainGroupPanelBorderStyleFor: aGroupPanel [
	"Answer the normal border style for a plain group panel."

	^SimpleBorderStyle new
		width: 1 scaledByDisplayScaleFactor;
		baseColor: Color transparent
]

{ #category : 'copying' }
UITheme >> postCopy [
	"Copy the settings too."

	super postCopy.
	self settings: self settings copy
]

{ #category : 'fill-styles' }
UITheme >> preferGradientFill [
	^ self settings preferGradientFill
]

{ #category : 'services' }
UITheme >> proceedIn: aThemedMorph text: aStringOrText title: aString [
	"Answer the result of a proceed dialog with the given label and title."

	^ ((ProceedDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText) openModal cancelled not
]

{ #category : 'border-styles' }
UITheme >> progressBarBorderStyle [
	"Return the progress bar borderStyle for a progress bar."

	| c |
	c := self progressBarColor.
	^ BorderStyle simple
		  width: 1 scaledByDisplayScaleFactor;
		  baseColor: c
]

{ #category : 'basic-colors' }
UITheme >> progressBarColor [
	"Answer the colour for the given progress bar."

	^ colorPalette progressBarColor
]

{ #category : 'fill-styles' }
UITheme >> progressBarFillColor [

	^ self basePassiveBackgroundColor
]

{ #category : 'fill-styles' }
UITheme >> progressBarProgressFillColor [

	^ self lightSelectionColor
]

{ #category : 'fill-styles' }
UITheme >> progressFillStyle [
	"Return the progress fillStyle for progress morph."

	^ self windowActiveFillStyle
]

{ #category : 'services' }
UITheme >> questionIn: aThemedMorph text: aStringOrText title: aString [
	"Answer the result of a question dialog with the given label and title."

	^ ((QuestionDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText) openModal answer
]

{ #category : 'services' }
UITheme >> questionWithoutCancelIn: aThemedMorph text: aStringOrText title: aString [
	"Answer the result of a question dialog with the given label and title."

	^ ((QuestionWithoutCancelDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText) openModal answer
]

{ #category : 'border-styles - buttons' }
UITheme >> radioButtonCornerStyleFor: aRadioButton [
	"Answer the corner style for radio buttons.
	Answer square when not disabled since the form is rounded."

	^aRadioButton enabled
		ifTrue: [#square]
		ifFalse: [#rounded]
]

{ #category : 'border-styles - buttons' }
UITheme >> radioButtonDisabledBorderStyleFor: aRadioButton [
	"Return the disabled radio button borderStyle for the given button."

	^self checkboxButtonDisabledBorderStyleFor: aRadioButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> radioButtonDisabledFillStyleFor: aRadioButton [
	"Return the disabled radio button fillStyle for the given button."

	^self radioButtonNormalFillStyleFor: aRadioButton
]

{ #category : 'fill-styles' }
UITheme >> radioButtonForm [

	^ self radioButtonFormSet asForm
]

{ #category : 'fill-styles' }
UITheme >> radioButtonFormSet [
	"Answer the form set to use for a normal radio button."

	^ self iconFormSetNamed: #radioButtonUnselected
]

{ #category : 'label-styles' }
UITheme >> radioButtonMarkerForm [
	"Answer the form to use for a radio button marker."

	^ self iconNamed: #radioButtonMarker
]

{ #category : 'border-styles - buttons' }
UITheme >> radioButtonNormalBorderStyleFor: aRadioButton [
	"Return the disabled radio button borderStyle for the given button."

	^self checkboxButtonNormalBorderStyleFor: aRadioButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> radioButtonNormalFillStyleFor: aRadioButton [
	"Return the normal radio button fillStyle for the given button."

	^(ImageFillStyle formSet: self radioButtonFormSet) origin: aRadioButton topLeft
]

{ #category : 'border-styles - buttons' }
UITheme >> radioButtonSelectedBorderStyleFor: aRadioButton [
	"Return the selected radio button borderStyle for the given button."

	^self radioButtonNormalBorderStyleFor: aRadioButton
]

{ #category : 'border-styles - buttons' }
UITheme >> radioButtonSelectedDisabledBorderStyleFor: aRadioButton [
	"Return the selecteddisabled radio button borderStyle for the given button."

	^self radioButtonDisabledBorderStyleFor: aRadioButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> radioButtonSelectedDisabledFillStyleFor: aRadioButton [
	"Return the selected disabled radio button fillStyle for the given button."

	^self radioButtonNormalFillStyleFor: aRadioButton
]

{ #category : 'fill-styles - buttons' }
UITheme >> radioButtonSelectedFillStyleFor: aRadioButton [
	"Return the selected radio button fillStyle for the given button."

	^(ImageFillStyle formSet: self radioButtonSelectedFormSet) origin: aRadioButton topLeft
]

{ #category : 'fill-styles' }
UITheme >> radioButtonSelectedForm [

	^ self radioButtonSelectedFormSet asForm
]

{ #category : 'fill-styles' }
UITheme >> radioButtonSelectedFormSet [
	"Answer the form set to use for a selected radio button."

	^ self iconFormSetNamed: #radioButtonSelected
]

{ #category : 'fill-styles' }
UITheme >> resizerGripNormalFillStyleFor: aResizer [

	^ colorPalette resizerGripNormalFillStyleFor: aResizer
]

{ #category : 'scrollbars' }
UITheme >> scrollBarButtonArrowVertices: aRectangle [
	^ self verticesForComplexArrow: aRectangle
]

{ #category : 'morph creation' }
UITheme >> scrollBarDownButtonBoundsFor: aScrollbar [

	| extent position |

	extent := aScrollbar buttonExtent.
	position := aScrollbar innerBounds bottomRight - extent.

	^ position extent: extent
]

{ #category : 'morph creation' }
UITheme >> scrollBarUpButtonBoundsFor: aScrollbar [
	^ aScrollbar innerBounds topLeft extent: aScrollbar buttonExtent
]

{ #category : 'border-styles' }
UITheme >> scrollPaneDisabledBorderStyleFor: aScrollPane [
	"Return the disabled borderStyle for the given scroll pane."

	^self scrollPaneNormalBorderStyleFor: aScrollPane
]

{ #category : 'border-styles' }
UITheme >> scrollPaneNormalBorderStyleFor: aScrollPane [
	"Return the normal borderStyle for the given scroll pane."

	^BorderStyle simple
		width: 1 scaledByDisplayScaleFactor;
		baseColor: aScrollPane paneColor
]

{ #category : 'label-styles' }
UITheme >> scrollbarArrowOfDirection: aSymbol size: finalSizeInteger color: aColor [
	"Answer a new scrollbar arrow form (normally cached by Scrollbar)."

	^self basicCreateArrowOfDirection: aSymbol size: finalSizeInteger color: aColor
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarButtonCornerStyleIn: aThemedMorph [
	"Allow for themes to override default behaviour."

	^#square
]

{ #category : 'accessing - colors' }
UITheme >> scrollbarColor [

	^ colorPalette scrollbarColor
]

{ #category : 'basic-colors' }
UITheme >> scrollbarColorFor: aScrollbar [

	"Answer the colour for the given scrollbar."

	^ colorPalette scrollbarColorFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarImageColorFor: aScrollbar [

	"Return the scrollbar image colour (on buttons) for the given scrollbar."

	^ colorPalette scrollbarImageColorFor: aScrollbar
]

{ #category : 'defaults' }
UITheme >> scrollbarMinimumThumbThickness [
	"Answer the minumum width or height of a scrollbar thumb
	as appropriate to its orientation."

	^15 * self displayScaleFactor
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarMouseOverBarButtonBorderStyleFor: aScrollbar [
	"Return the button borderStyle for the given scrollbar when
	the mouse is over the bar."

	^self scrollbarNormalButtonBorderStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarMouseOverBarButtonFillStyleFor: aScrollbar [
	"Return the button fillStyle for the given scrollbar when
	the mouse is over the bar."

	^self scrollbarNormalButtonFillStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarMouseOverBarThumbBorderStyleFor: aScrollbar [
	"Return the thumb borderStyle for the given scrollbar when
	the mouse is over the bar."

	^self scrollbarNormalThumbBorderStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarMouseOverBarThumbFillStyleFor: aScrollbar [
	"Return the thumb fillStyle for the given scrollbar when
	the mouse is over the bar."

	^self scrollbarNormalThumbFillStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarMouseOverBorderStyleFor: aScrollbar [
	"Return the mouse over borderStyle for the given scrollbar."

	^self scrollbarNormalBorderStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarMouseOverButtonBorderStyleFor: aScrollbar [
	"Return the mouse over button borderStyle for the given scrollbar."

	^self scrollbarNormalButtonBorderStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarMouseOverButtonFillStyleFor: aScrollbar [
	"Return the scrollbar mouse over button fillStyle for the given color."

	^self scrollbarNormalButtonFillStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarMouseOverFillStyleFor: aScrollbar [
	"Return the scrollbar mouse over fillStyle for the given color."

	^self scrollbarNormalFillStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarMouseOverThumbBorderStyleFor: aScrollbar [
	"Return the mouse over thumb borderStyle for the given scrollbar."

	^self scrollbarNormalThumbBorderStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarMouseOverThumbButtonBorderStyleFor: aScrollbar [
	"Return the button borderStyle for the given scrollbar when
	the mouse is over the thumb."

	^self scrollbarMouseOverBarButtonBorderStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarMouseOverThumbButtonFillStyleFor: aScrollbar [
	"Return the button fillStyle for the given scrollbar when
	the mouse is over the thumb."

	^self scrollbarMouseOverBarButtonFillStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarMouseOverThumbFillStyleFor: aScrollbar [
	"Return the scrollbar mouse over thumb fillStyle for the given color."

	^self scrollbarNormalThumbFillStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarNormalBorderStyleFor: aScrollbar [
	"Return the normal button borderStyle for the given scrollbar."

	^BorderStyle simple
		width: 0;
		baseColor: Color transparent
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarNormalButtonBorderStyleFor: aScrollbar [
	"Return the normal button borderStyle for the given scrollbar."

	^BorderStyle simple
		width: 0
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarNormalButtonFillStyleFor: aScrollbar [
	"Return the normal scrollbar button fillStyle for the given scrollbar."

	^ self scrollbarNormalThumbFillStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarNormalFillStyleFor: aScrollbar [

	^ colorPalette scrollbarNormalFillStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarNormalThumbBorderStyleFor: aScrollbar [
	"Return the normal thumb borderStyle for the given scrollbar."

	^ BorderStyle simple
		width: 1 scaledByDisplayScaleFactor;
		baseColor: Color transparent
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarNormalThumbFillStyleFor: aScrollbar [
	"Return the normal scrollbar fillStyle for the given scrollbar."
	^ self normalFillStyleFor: aScrollbar height: aScrollbar height
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarPagingAreaCornerStyleIn: aThemedMorph [
	^#square
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarPressedBorderStyleFor: aScrollbar [
	"Return the pressed borderStyle for the given scrollbar."

	^self scrollbarNormalBorderStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarPressedButtonBorderStyleFor: aScrollbar [
	"Return the pressed button borderStyle for the given scrollbar."
	^ self scrollbarNormalButtonBorderStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarPressedButtonFillStyleFor: aScrollbar [
	"Return the pressed scrollbar button fillStyle for the given scrollbar."

	^self scrollbarPressedThumbFillStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarPressedFillStyleFor: aScrollbar [
	"Return the pressed scrollbar fillStyle for the given scrollbar."

	^self scrollbarNormalFillStyleFor: aScrollbar
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarPressedThumbBorderStyleFor: aScrollbar [
	"Return the pressed thumb borderStyle for the given scrollbar."

	^self scrollbarNormalThumbBorderStyleFor: aScrollbar
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> scrollbarPressedThumbFillStyleFor: aScrollbar [
	"Return the pressed scrollbar thumb fillStyle for the given scrollbar."

	^self scrollbarNormalThumbFillStyleFor: aScrollbar
]

{ #category : 'defaults' }
UITheme >> scrollbarThickness [
	"Answer the width or height of a scrollbar as appropriate to
	its orientation."

	^13 * self displayScaleFactor
]

{ #category : 'border-styles - scrollbars' }
UITheme >> scrollbarThumbCornerStyleIn: aThemedMorph [
	"Allow for themes to override default behaviour."

	^#square
]

{ #category : 'accessing - colors' }
UITheme >> secondarySelectionColor [

	^ colorPalette secondarySelectionColor
]

{ #category : 'private' }
UITheme >> selectedFillStyleFor: aMorph height: anInteger [

	^ SolidFillStyle color: self lightSelectionColor
]

{ #category : 'accessing - colors' }
UITheme >> selectedItemListTextColor [

	"Answer the color for the selected item in a list text."

	^ colorPalette selectedItemListTextColor
]

{ #category : 'accessing - colors' }
UITheme >> selectionBarColor [

	^ colorPalette selectionBarColor
]

{ #category : 'accessing - colors' }
UITheme >> selectionColor [

	"Answer the selection color to use.."

	^ colorPalette selectionColor
]

{ #category : 'accessing - colors' }
UITheme >> selectionTextColor [

	^ colorPalette selectionTextColor
]

{ #category : 'fill-styles' }
UITheme >> separatorFillStyleFor: aSeparator [
	"Return the separator fillStyle for the given separator."

	^ SolidFillStyle color: (self baseColorFor: aSeparator) darker
]

{ #category : 'defaults' }
UITheme >> setSystemProgressMorphDefaultParameters: aProgressMorph [
	"Set up the given progress morph."
	aProgressMorph color: self settings derivedMenuColor.
	self settings preferRoundCorner
		ifTrue: [aProgressMorph useRoundedCorners].
	aProgressMorph borderWidth: self menuBorderWidth.
	aProgressMorph borderColor: self menuBorderColor.
	aProgressMorph updateColor
]

{ #category : 'accessing' }
UITheme >> settings [
	"Answer the value of settings"

	^ settings
]

{ #category : 'accessing' }
UITheme >> settings: anObject [
	"Set the value of settings"

	settings := anObject
]

{ #category : 'accessing' }
UITheme >> shStyleTable [

	"Styling depends on shout. If it is not present, means there is no style
	 table to apply"
	^ (self class environment classNamed: #SHRBTextStyler)
		ifNotNil: [ :aClass | aClass perform: self shStyleTableName ]
		ifNil: [ #() ]
]

{ #category : 'accessing' }
UITheme >> shStyleTableName [

	^ #blueStyleTable
]

{ #category : 'testing' }
UITheme >> shouldUseAnimations [
	^ self animationSettings useAnimation
]

{ #category : 'border-styles' }
UITheme >> sliderDisabledBorderStyleFor: aSlider [
	"Return the disabled slider borderStyle for the given text editor."

	^self sliderNormalBorderStyleFor: aSlider
]

{ #category : 'fill-styles' }
UITheme >> sliderDisabledFillStyleFor: aSlider [
	"Return the disabled fillStyle for the given slider."

	^ self textEditorDisabledFillStyleFor: aSlider
]

{ #category : 'border-styles' }
UITheme >> sliderNormalBorderStyleFor: aSlider [
	"Return the normal slider borderStyle for the given text editor."

	^BorderStyle inset
		width: 1 scaledByDisplayScaleFactor;
		baseColor: aSlider paneColor
]

{ #category : 'fill-styles' }
UITheme >> sliderNormalFillStyleFor: aSlider [

	"Return the normal fillStyle for the given slider."

	^ colorPalette sliderNormalFillStyleFor: aSlider
]

{ #category : 'fill-styles' }
UITheme >> splitterNormalFillStyleFor: aSplitter [
	"Return the normal splitter fillStyle for the given splitter."

	^ SolidFillStyle color: Color transparent
]

{ #category : 'fill-styles' }
UITheme >> splitterPressedFillStyleFor: aSplitter [
	"Return the pressed splitter fillStyle for the given splitter."

	|aColor|
	aColor := self baseColorFor: aSplitter.
	^ (GradientFillStyle ramp: {0.0->aColor lighter. 0.9-> aColor})
		origin: aSplitter topLeft;
		direction: (aSplitter splitsTopAndBottom
			ifTrue: [0 @ aSplitter height]
			ifFalse: [aSplitter width @ 0]);
		radial: false
]

{ #category : 'accessing - colors' }
UITheme >> spotlightWindowColor [

	^ colorPalette spotlightWindowColor
]

{ #category : 'basic-colors' }
UITheme >> subgroupColorFrom: paneColor [

	"Answer the colour for a subgroup given the pane colour."

	^ colorPalette subgroupColorFrom: paneColor
]

{ #category : 'accessing - colors' }
UITheme >> successBackgroundColor [

	^ colorPalette successBackgroundColor
]

{ #category : 'accessing - colors' }
UITheme >> successTextColor [

	^ colorPalette successTextColor
]

{ #category : 'border-styles' }
UITheme >> tabGroupCornerStyleIn: aThemedMorph [
	"Allow for themes to override default behaviour."

	^aThemedMorph
		ifNil: [#square]
		ifNotNil: [:tm | tm preferredCornerStyle]
]

{ #category : 'defaults' }
UITheme >> tabLabelInsetFor: aButton [
	"Answer the inset to use for a tab label."
	| left top right bottom |
	left := right := 8 scaledByDisplayScaleFactor.
	top := bottom := 2 scaledByDisplayScaleFactor.
	^ Margin left: left top: top right: right bottom: bottom
]

{ #category : 'border-styles' }
UITheme >> tabLabelNormalBorderStyleFor: aTabLabel [
"	^SimpleBorder new
		width: 0;
		baseColor: (self buttonBaseColorFor: aTabLabel) darker
"	^ self buttonNormalBorderStyleFor: aTabLabel
]

{ #category : 'fill-styles - buttons' }
UITheme >> tabLabelNormalFillStyleFor: aTabLabel [
	^ self buttonNormalFillStyleFor: aTabLabel
]

{ #category : 'border-styles' }
UITheme >> tabLabelSelectedBorderStyleFor: aTabLabel [
	"Answer the selected border style for a tab label."

	^self tabLabelNormalBorderStyleFor: aTabLabel
]

{ #category : 'fill-styles - buttons' }
UITheme >> tabLabelSelectedFillStyleFor: aTabLabel [
	^ self buttonSelectedFillStyleFor: aTabLabel
]

{ #category : 'border-styles' }
UITheme >> tabPanelBorderStyleFor: aTabGroup [
	^ Pharo3TabPanelBorderStyle new
		width: 1 scaledByDisplayScaleFactor;
		baseColor: ((self darkBaseColorFor: aTabGroup));
		tabSelector: aTabGroup tabSelectorMorph
]

{ #category : 'defaults' }
UITheme >> tabSelectorCellInsetFor: aTabSelector [
	"Answer the cell inset to use for the given tab selector."

	^0
]

{ #category : 'defaults' }
UITheme >> tabSelectorMorphMinExtentFor: aTabSelectorMorph [
	"Answer the min extent of the given tab selector."

	^aTabSelectorMorph basicMinExtent + (8 @ 1) scaledByDisplayScaleFactor
]

{ #category : 'fill-styles' }
UITheme >> taskListFillStyleFor: aTasklist [

	| aColor result |
	aColor := aTasklist color.
	result := GradientFillStyle ramp: {
			0.0->aColor whiter whiter. 0.2->aColor lighter.
			0.8->aColor darker. 1.0->aColor blacker}.
	^ result
		origin: aTasklist topLeft;
		direction: 0 @ aTasklist height;
		radial: false;
		yourself
]

{ #category : 'basic-colors' }
UITheme >> taskbarActiveButtonColorFor: aButton [

	"Answer the colour for the given active taskbar button."

	^ colorPalette taskbarActiveButtonColorFor: aButton
]

{ #category : 'basic-colors' }
UITheme >> taskbarButtonColorFor: aButton [

	"Answer the colour for the given taskbar button."

	^ colorPalette taskbarButtonColorFor: aButton
]

{ #category : 'basic-colors' }
UITheme >> taskbarButtonLabelColorFor: aButton [

	"Answer the colour for the label of the given taskbar button."

	^ colorPalette taskbarButtonLabelColorFor: aButton
]

{ #category : 'fill-styles - taskbar' }
UITheme >> taskbarFillStyleFor: aTaskbar [
	^ SolidFillStyle color: Color transparent
]

{ #category : 'border-styles - taskbar' }
UITheme >> taskbarItemDisabledBorderStyleFor: aButton [
	"Return the disabled taskbar item borderStyle for the given button."

	^ self buttonDisabledBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
UITheme >> taskbarItemDisabledFillStyleFor: aButton [
	"Return the disabled taskbar item fillStyle for the given color."

	^ self buttonDisabledFillStyleFor: aButton
]

{ #category : 'basic-colors' }
UITheme >> taskbarItemLabelColorForCollapsed: aButton [

	"Answer the colour for the label of the given taskbar button, given that the corresponding
	window is collapsed."

	^ colorPalette taskbarItemLabelColorForCollapsed: aButton
]

{ #category : 'basic-colors' }
UITheme >> taskbarItemLabelColorForExpanded: aButton [

	"Answer the colour for the label of the given taskbar button, given that the corresponding
	window is collapsed."

	^ colorPalette taskbarItemLabelColorForExpanded: aButton
]

{ #category : 'border-styles - taskbar' }
UITheme >> taskbarItemMouseOverBorderStyleFor: aButton [
	"Return the mouse over taskbar item borderStyle for the given button."

	^ self buttonMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
UITheme >> taskbarItemMouseOverFillStyleFor: aButton [
	"Return the taskbar item mouse over fillStyle for the given color."

	^ self buttonMouseOverFillStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
UITheme >> taskbarItemNormalBorderStyleFor: aButton [
	"Return the normal taskbar item borderStyle for the given button."

	^ self buttonNormalBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
UITheme >> taskbarItemNormalFillStyleFor: aButton [
	"Return the normal taskbar item fillStyle for the given button."

	^ self buttonNormalFillStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
UITheme >> taskbarItemPressedBorderStyleFor: aButton [
	"Return the pressed taskbar item borderStyle for the given button."

	^ self buttonPressedBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
UITheme >> taskbarItemPressedFillStyleFor: aButton [
	"Return the taskbar item pressed fillStyle for the given button."

	^ self buttonPressedFillStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
UITheme >> taskbarItemSelectedBorderStyleFor: aButton [
	"Return the selected taskbar item borderStyle for the given button."

	^ self buttonSelectedBorderStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
UITheme >> taskbarItemSelectedDisabledBorderStyleFor: aButton [
	"Return the selecteddisabled taskbar item borderStyle for the given button."

	^ self buttonSelectedDisabledBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
UITheme >> taskbarItemSelectedDisabledFillStyleFor: aButton [
	"Return the taskbar item selected disabled fillStyle for the given color."

	^ self buttonSelectedDisabledFillStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
UITheme >> taskbarItemSelectedFillStyleFor: aButton [
	"Return the taskbar item button fillStyle for the given button."

	^ self buttonSelectedFillStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
UITheme >> taskbarItemSelectedMouseOverBorderStyleFor: aButton [
	"Return the selected mouse over taskbar item borderStyle for the given button."

	^ self buttonSelectedMouseOverBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
UITheme >> taskbarItemSelectedMouseOverFillStyleFor: aButton [
	"Return the taskbar item selected mouse over fillStyle for the given color."

	^ self buttonSelectedMouseOverFillStyleFor: aButton
]

{ #category : 'border-styles - taskbar' }
UITheme >> taskbarItemSelectedPressedBorderStyleFor: aButton [
	"Return the selected pressed taskbar item borderStyle for the given button."

	^ self buttonSelectedPressedBorderStyleFor: aButton
]

{ #category : 'fill-styles - taskbar' }
UITheme >> taskbarItemSelectedPressedFillStyleFor: aButton [
	"Return the taskbar item selected pressed fillStyle for the given color."

	^ self buttonSelectedPressedFillStyleFor: aButton
]

{ #category : 'basic-colors' }
UITheme >> taskbarMinimizedButtonColorFor: aButton [

	"Answer the colour for the given minimized taskbar button."

	^ colorPalette taskbarMinimizedButtonColorFor: aButton
]

{ #category : 'border-styles' }
UITheme >> taskbarThumbnailCornerStyleFor: aMorph [
	"Answer the corner style for the taskbar thumbnail/tasklist."

	^#square
]

{ #category : 'border-styles' }
UITheme >> taskbarThumbnailNormalBorderStyleFor: aWindow [
	^ self buttonNormalBorderStyleFor: aWindow
]

{ #category : 'accessing - colors' }
UITheme >> textColor [

	^ colorPalette textColor
]

{ #category : 'accessing - colors' }
UITheme >> textColorForNonStandardBackground [

	^ colorPalette textColorForNonStandardBackground
]

{ #category : 'border-styles' }
UITheme >> textEditorDisabledBorderStyleFor: aTextEditor [
	"Return the disabled text editor borderStyle for the given text editor."

	^self textEditorNormalBorderStyleFor: aTextEditor
]

{ #category : 'fill-styles' }
UITheme >> textEditorDisabledFillStyleFor: aTextEditor [
	"Return the disabled fillStyle for the given text editor."

	^ self basePassiveBackgroundColor
]

{ #category : 'services' }
UITheme >> textEditorIn: aThemedMorph text: aStringOrText title: aString entryText: defaultEntryText entryHeight: entryHeight [
	"Answer the result of a text editor (multiline) dialog ( a string or nil if cancelled)
	with the given label and title."

	^ ((TextEditorDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText;
		   entryText: defaultEntryText;
		   entryHeight: entryHeight) openModal entryText
]

{ #category : 'border-styles' }
UITheme >> textEditorNormalBorderStyleFor: aTextEditor [
	"Return the normal text editor borderStyle for the given text editor."

	^self buttonNormalBorderStyleFor: aTextEditor
]

{ #category : 'fill-styles' }
UITheme >> textEditorNormalFillStyleFor: aTextEditor [

	"Return the normal fillStyle for the given text editor."

	^ colorPalette textEditorNormalFillStyleFor: aTextEditor
]

{ #category : 'border-styles - buttons' }
UITheme >> textEntryCornerStyleIn: aThemedMorph [
	"Answer the corner style to use for text entry morphs."

	^#square
]

{ #category : 'services' }
UITheme >> textEntryIn: aThemedMorph text: aStringOrText title: aString entryText: defaultEntryText [
	"Answer the result of a text entry dialog ( a string or nil if cancelled)
	with the given label and title."

	^ (TextEntryDialogWindow new
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText;
		   entryText: defaultEntryText) openModal entryText
]

{ #category : 'services' }
UITheme >> textEntryIn: aThemedMorph text: aStringOrText title: aString entryText: defaultEntryText entryCompletion: anEntryCompletion [
	"Answer the result of a text entry dialog ( a string or nil if cancelled)
	with the given label and title."

	^ ((TextEntryDialogWindow newWithTheme: aThemedMorph theme)
		   textFont: self textFont;
		   title: aString;
		   text: aStringOrText;
		   entryText: defaultEntryText;
		   entryCompletion: anEntryCompletion) openModal entryText
]

{ #category : 'fill-styles' }
UITheme >> textFieldNormalFillStyleFor: aTextField [

	^ colorPalette textFieldNormalFillStyleFor: aTextField
]

{ #category : 'defaults' }
UITheme >> textFont [
	"Answer the text font to use."

	^StandardFonts defaultFont
]

{ #category : 'label-styles' }
UITheme >> treeExpandedForm [

	^ self treeExpandedFormSet asForm
]

{ #category : 'label-styles' }
UITheme >> treeExpandedFormSet [
	"Answer the form set to use for an expanded tree item."

	^ self formSetsForCurrentDisplayScaleFactor at: #treeExpanded
]

{ #category : 'basic-colors' }
UITheme >> treeLineColorsFrom: aColor [

	"Answer the colours to use for the tree lines.
	The given colour is the preferred colour."

	^ colorPalette treeLineColorsFrom: aColor
]

{ #category : 'basic-colors' }
UITheme >> treeLineDashes [
	"Answer the dash lengths to use for the tree lines."

	| length |
	length := 1 scaledByDisplayScaleFactor.
	^ { length. length }
]

{ #category : 'defaults' }
UITheme >> treeLineWidth [
	"Answer the width of the tree lines."

	^0
]

{ #category : 'label-styles' }
UITheme >> treeUnexpandedForm [

	^ self treeUnexpandedFormSet asForm
]

{ #category : 'label-styles' }
UITheme >> treeUnexpandedFormSet [
	"Answer the form set to use for an unexpanded tree item."

	^ self formSetsForCurrentDisplayScaleFactor at: #treeUnexpanded
]

{ #category : 'accessing - colors' }
UITheme >> unfocusedSelectionColor [

	^ colorPalette unfocusedSelectionColor
]

{ #category : 'scrollbars' }
UITheme >> updateScrollBarButtonAspect: aMorph color: aColor [

	"update aMorph with aColor"

	aMorph ifNil: [ ^ self ].
	aMorph color: aColor
]

{ #category : 'initialization' }
UITheme >> updateWorldDockingBars [
	"Update the world docking bar gradients."

	self currentWorld mainDockingBars do: [:d |
		d
			color: (self worldMainDockingBarColorFor: d);
			gradientRamp: (self class current
				worldMainDockingBarNormalFillStyleFor: d) colorRamp;
			borderStyle: (self worldMainDockingBarBorderStyleFor: d)]
]

{ #category : 'accessing - colors' }
UITheme >> urlColor [

	^ colorPalette urlColor
]

{ #category : 'accessing' }
UITheme >> useAnimation: aBoolean [
	^ self animationSettings useAnimation: aBoolean
]

{ #category : 'fill-styles - scrollbars' }
UITheme >> useScrollbarThumbShadow [
	"Answer whether a shadow morph should be displayed when
	dragging a scrollbar thumb."

	"If for some reason it's nil, reinitialice to 'false'."

	useScrollbarThumbShadow ifNil:[useScrollbarThumbShadow:=false].

	^useScrollbarThumbShadow
]

{ #category : 'scrollbars' }
UITheme >> verticesForComplexArrow: aRectangle [
	"PRIVATE - answer a collection of vertices to draw a complex arrow"

	|r vertices aux b c|
	r := aRectangle origin extent: aRectangle width asPoint.
	aux := (r width / 5) rounded.
	b := (aux * aux * 2) sqrt.
	c := (b * b * 2) sqrt rounded.
	vertices := OrderedCollection new.
	vertices add: r bottomLeft - (0 @ aux).
	vertices add: r topCenter + (0 @ aux).
	vertices add: r bottomRight - (0 @ aux).
	vertices add: r bottomRight - (aux @ 0).
	vertices add: r topCenter + (0 @ (aux + c)).
	vertices add: r bottomLeft + (aux @ 0).
	^vertices
]

{ #category : 'basic-colors' }
UITheme >> veryLightSelectionColor [

	^ colorPalette veryLightSelectionColor
]

{ #category : 'private' }
UITheme >> veryLightSelectionColorFor: aMorph [

	^ colorPalette veryLightSelectionColor
]

{ #category : 'accessing - colors' }
UITheme >> warningBackgroundColor [

	^ colorPalette warningBackgroundColor
]

{ #category : 'accessing - colors' }
UITheme >> warningTextColor [

	^ colorPalette warningTextColor
]

{ #category : 'label-styles' }
UITheme >> whiteTreeExpandedForm [

	^ self whiteTreeExpandedFormSet asForm
]

{ #category : 'label-styles' }
UITheme >> whiteTreeExpandedFormSet [
	"Answer the form set to use for an expanded tree item when a contrasting one is needed."

	^ self formSetsForCurrentDisplayScaleFactor at: #whiteTreeExpanded
]

{ #category : 'label-styles' }
UITheme >> whiteTreeUnexpandedForm [

	^ self whiteTreeUnexpandedFormSet asForm
]

{ #category : 'label-styles' }
UITheme >> whiteTreeUnexpandedFormSet [
	"Answer the form set to use for an unexpanded tree item when a contrasting one is needed."

	^ self formSetsForCurrentDisplayScaleFactor at: #whiteTreeUnexpanded
]

{ #category : 'border-styles' }
UITheme >> windowActiveDropShadowOffsetRectangleFor: aSystemWindow [
	"Answer a rectangle describing the offsets for each corner
	of the the active window bounds for the drop shadow area."

	^self windowActiveDropShadowStyle == #diffuse
		ifTrue: [self windowDiffuseDropShadowOffsetRectangleFor: aSystemWindow]
		ifFalse: [self windowPlainDropShadowOffsetRectangleFor: aSystemWindow]
]

{ #category : 'accessing' }
UITheme >> windowActiveDropShadowStyle [
	"Answer the style of drop shadow to use for active windows."

	^windowActiveDropShadowStyle
]

{ #category : 'accessing' }
UITheme >> windowActiveDropShadowStyle: anObject [
	"Set the value of windowActiveDropShadowStyle"

	windowActiveDropShadowStyle := anObject
]

{ #category : 'fill-styles' }
UITheme >> windowActiveFillStyle [
	"We do not want the lighting effect when the window goes inactive"

	^ SolidFillStyle color: self baseColor
]

{ #category : 'fill-styles' }
UITheme >> windowActiveLabelFillStyleFor: aWindow [

	"Return the window active label fillStyle for the given window."

	^ colorPalette windowActiveLabelFillStyleFor: aWindow
]

{ #category : 'fill-styles' }
UITheme >> windowActiveTitleFillStyleFor: aWindow [
	^ SolidFillStyle color: Color transparent
]

{ #category : 'defaults' }
UITheme >> windowBorderWidthFor: aWindow [

	^ 4 * self displayScaleFactor
]

{ #category : 'defaults' }
UITheme >> windowBottomOffset [

	^ 0
]

{ #category : 'label-styles - windows' }
UITheme >> windowCloseForm [

	^ self windowCloseFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowCloseFormSet [
	"Answer the form set to use for the close button of a window."

	^ self iconFormSetNamed: #windowClose
]

{ #category : 'label-styles - windows' }
UITheme >> windowCloseOverForm [

	^ self windowCloseOverFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowCloseOverFormSet [
	"Answer the form set to use for mouse over window close buttons"

	^ self iconFormSetNamed: #windowClose
]

{ #category : 'label-styles - windows' }
UITheme >> windowClosePassiveForm [

	^ self windowClosePassiveFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowClosePassiveFormSet [
	"Answer the form set to use for passive (background) window close buttons"

	^ self iconFormSetNamed: #windowCloseInactive
]

{ #category : 'morph creation' }
UITheme >> windowCollapseBoxLayoutFor: aWindow [

	^ LayoutFrame new
		leftFraction: 1;
		leftOffset: aWindow boxExtent x negated;
		yourself
]

{ #category : 'accessing - colors' }
UITheme >> windowColor [

	"Answer the window color to use."

	^ colorPalette windowColor
]

{ #category : 'basic-colors' }
UITheme >> windowColorFor: aWindowOrModel [
	"Answer the colour for the given window."

	^ colorPalette windowColorFor: aWindowOrModel
]

{ #category : 'border-styles' }
UITheme >> windowDiffuseDropShadowOffsetRectangleFor: aSystemWindow [
	"Answer a rectangle describing the offsets for each corner
	of the the active window bounds for the drop shadow area
	when the difuse style is used."

	^ (8@4) scaledByDisplayScaleFactor corner: (8@10) scaledByDisplayScaleFactor
]

{ #category : 'fill-styles' }
UITheme >> windowEdgeNormalFillStyleFor: anEdgeGrip [

	^ colorPalette windowEdgeNormalFillStyleFor: anEdgeGrip
]

{ #category : 'fill-styles' }
UITheme >> windowEdgePressedFillStyleFor: anEdgeGrip [
	"Return the pressed window edge fillStyle for the given edge grip."

	^self windowEdgeNormalFillStyleFor: anEdgeGrip
]

{ #category : 'fill-styles' }
UITheme >> windowExtentChangedFor: aWindow [
	"Update any extent related visuals."
]

{ #category : 'fill-styles' }
UITheme >> windowInactiveFillStyleFor: aWindow [
	"We do not want the lighting effect when the window goes inactive"

	^ self windowActiveFillStyle
]

{ #category : 'fill-styles' }
UITheme >> windowInactiveLabelFillStyleFor: aWindow [
	"Return the window inactive label fillStyle for the given window."

	^ self fadedBackgroundWindows
		ifTrue: [ aWindow paneColorToUse alphaMixed: 0.6 with: Color black ]
		ifFalse: [ self windowActiveLabelFillStyleFor: aWindow ]
]

{ #category : 'fill-styles' }
UITheme >> windowInactiveTitleFillStyleFor: aWindow [
	"We do not want the lighting effect when the window goes inactive"
	^ SolidFillStyle color: Color transparent
]

{ #category : 'label-styles - windows' }
UITheme >> windowLabelFor: aWindow [
	"Answer the label to use for the given window."

	^self windowLabelForText: aWindow labelString
]

{ #category : 'label-styles - windows' }
UITheme >> windowLabelForText: aTextOrString [
	"Answer the window label to use for the given text."
	^ LabelMorph new
		contents: aTextOrString;
		font: StandardFonts windowTitleFont emphasis: 0
]

{ #category : 'defaults' }
UITheme >> windowLeftOffset [

	^ 0
]

{ #category : 'label-styles - windows' }
UITheme >> windowMaximizeForm [

	^ self windowMaximizeFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowMaximizeFormSet [
	"Answer the form set to use for the maximize button of a window."

	^ self iconFormSetNamed: #windowMaximize
]

{ #category : 'label-styles - windows' }
UITheme >> windowMaximizeOverForm [

	^ self windowMaximizeOverFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowMaximizeOverFormSet [
	"Answer the form set to use for mouse over window maximize buttons"

	^ self iconFormSetNamed: #windowMaximize
]

{ #category : 'label-styles - windows' }
UITheme >> windowMaximizePassiveForm [

	^ self windowMaximizePassiveFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowMaximizePassiveFormSet [
	"Answer the form set to use for passive (background) window maximize/restore buttons"

	^ self iconFormSetNamed: #windowMaximizeInactive
]

{ #category : 'label-styles - windows' }
UITheme >> windowMenuForm [

	^ self windowMenuFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowMenuFormSet [
	"Answer the form set to use for the menu button of a window."

	^ self iconFormSetNamed: #windowMenu
]

{ #category : 'label-styles - windows' }
UITheme >> windowMenuIconFor: aWindow [
	"Answer the menu icon for the given window."

	^ self windowMenuForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowMenuPassiveForm [

	^self windowMenuPassiveFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowMenuPassiveFormSet [
	"Answer the form set to use for passive (background) window menu buttons"

	^self newWindowMenuPassiveFormSet
]

{ #category : 'label-styles - windows' }
UITheme >> windowMinimizeForm [

	^ self windowMinimizeFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowMinimizeFormSet [
	"Answer the form set to use for the minimize button of a window."

	^ self iconFormSetNamed: #windowMinimize
]

{ #category : 'label-styles - windows' }
UITheme >> windowMinimizeOverForm [

	^ self windowMinimizeOverFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowMinimizeOverFormSet [
	"Answer the form set to use for mouse over window minimize buttons"

	^ self iconFormSetNamed: #windowMinimize
]

{ #category : 'label-styles - windows' }
UITheme >> windowMinimizePassiveForm [

	^ self windowMinimizePassiveFormSet asForm
]

{ #category : 'label-styles - windows' }
UITheme >> windowMinimizePassiveFormSet [
	"Answer the form set to use for passive (background) window minimize buttons"

	^ self iconFormSetNamed: #windowMinimizeInactive
]

{ #category : 'border-styles' }
UITheme >> windowPaneBorderStyleFor: aMorph in: aSystemWindow [
	"Answer the border style for a morph that is to be added
	as a pane in the given system window,"

	^BorderStyle simple
		color: aSystemWindow paneColor;
		width: (aMorph borderWidth = 0 ifTrue: [0] ifFalse: [1 scaledByDisplayScaleFactor])
]

{ #category : 'border-styles' }
UITheme >> windowPlainDropShadowOffsetRectangleFor: aSystemWindow [
	"Answer a rectangle describing the offsets for each corner
	of the the active window bounds for the drop shadow area
	when the plain style is used."

	^aSystemWindow shadowOffset negated corner: aSystemWindow shadowOffset
]

{ #category : 'defaults' }
UITheme >> windowPreferredCornerStyleFor: aWindow [
	"Answer the preferred corner style for the given window."

	aWindow isEmbedded ifTrue: [^#square].
	^(self settings preferRoundCorner  or: [
			aWindow cornerStyle == #rounded])
		ifTrue: [#rounded]
		ifFalse: [#square]
]

{ #category : 'defaults' }
UITheme >> windowRightOffset [

	^ 0
]

{ #category : 'accessing - colors' }
UITheme >> windowShadowColor [

	"Answer the window shadow color to use."

	^ colorPalette windowShadowColor
]

{ #category : 'defaults' }
UITheme >> windowTopOffset [

	^ 0
]

{ #category : 'border-styles' }
UITheme >> worldMainDockingBarBorderStyleFor: aDockingBar [
	"Return the world main docking bar borderStyle for the given docking bar."

	^BorderStyle simple
		width: 0;
		color: Color transparent
]

{ #category : 'basic-colors' }
UITheme >> worldMainDockingBarColorFor: aDockingBar [

	"Answer the base colour to use for a world main docking bar in this theme."

	^ colorPalette worldMainDockingBarColorFor: aDockingBar
]

{ #category : 'fill-styles' }
UITheme >> worldMainDockingBarNormalFillStyleFor: aDockingBar [
	"Return the world main docking bar fillStyle for the given docking bar."

	|aColor|
	aColor := aDockingBar originalColor alpha: 0.7.
	^(GradientFillStyle ramp: {
			0.0->(aColor alphaMixed: 0.3 with: (Color white alpha: aColor alpha)).
			0.8->aColor darker.
			1.0->aColor darker duller})
		origin: aDockingBar topLeft;
		direction: (aDockingBar isVertical
			ifTrue: [aDockingBar width @ 0]
			ifFalse: [0 @ aDockingBar height]);
		radial: false
]

Extension { #name : 'Morph' }

{ #category : '*Morphic-Widgets' }
Morph >> activate [
	"Mark the receiver and submorphs as active (foreground)."

	self submorphsDo: [:m | m activate]
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> allowsKeymapping [

	^ self valueOfProperty: #allowsKeymapping ifAbsent: [ true]
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> allowsKeymapping: aBoolean [

	^ self setProperty: #allowsKeymapping toValue: aBoolean
]

{ #category : '*Morphic-Widgets' }
Morph >> asText [

	| anchor embedMorphSignal |
    anchor := TextAnchor new anchoredMorph: self.
    embedMorphSignal := (Character value: 1) asString. "required by the scanner"
    ^ Text string: embedMorphSignal attribute: anchor
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> attachKeymapCategory: aCategory [
	self kmDispatcher attachCategory: aCategory
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> attachKeymapCategory: aCategoryName onProperty: aProperty [
	self kmDispatcher attachCategory: aCategoryName onProperty: aProperty
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> attachKeymapCategory: aCategoryName targetting: anObject [
	self kmDispatcher attachCategory: aCategoryName targetting: anObject
]

{ #category : '*Morphic-Widgets' }
Morph >> basicTheme: aUITheme [
	"Set the current theme for the receiver without notification of change."

	self theme = aUITheme ifFalse: [
		self setProperty: #theme toValue: aUITheme]
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> bindKeyCombination: aShortcut toAction: anAction [
	
	self kmDispatcher 
		bindKeyCombination: aShortcut asKeyCombination 
		toAction: anAction
]

{ #category : '*Morphic-Widgets' }
Morph >> borderColor: aColorOrSymbolOrNil [
	"Unfortunately, the argument to borderColor could be more than 	just a color.
	It could also be a symbol, in which case it is to be interpreted as a style identifier.
	But I might not be able to draw that kind of border, so it may have to be ignored.
	Or it could be nil, in which case I should revert to the default border."

	| style newStyle |
	style := self borderStyle.
	style baseColor = aColorOrSymbolOrNil
		ifTrue: [^ self].

	aColorOrSymbolOrNil isColor
		ifTrue: [style style = #none "default border?"
				ifTrue: [self borderStyle: (SimpleBorderStyle width: 0 color: aColorOrSymbolOrNil)]
				ifFalse: [style baseColor: aColorOrSymbolOrNil.
					self changed].
			^ self].

	self
		borderStyle: ( ({ nil. #none } includes: aColorOrSymbolOrNil)
				ifTrue: [BorderStyle default]
				ifFalse: [ "a symbol"
					self doesBevels ifFalse: [ ^self ].
					newStyle := (BorderStyle perform: aColorOrSymbolOrNil)
								color: style color;
								width: style width;
								yourself.
					(self canDrawBorder: newStyle)
						ifTrue: [newStyle]
						ifFalse: [style]])
]

{ #category : '*Morphic-Widgets' }
Morph >> borderWidth: aNumber [
	| style |
	style := self borderStyle.
	style width = aNumber ifTrue: [ ^self ].

	style style = #none
		ifTrue: [ self borderStyle: (SimpleBorderStyle width: aNumber color: Color transparent) ]
		ifFalse: [ style width: aNumber. self changed ]
]

{ #category : '*Morphic-Widgets' }
Morph >> changeProportionalLayout [
	| layout |
	((layout := self layoutPolicy) isNotNil and:[layout isProportionalLayout])
		ifTrue:[^self]. "already proportional layout"
	self layoutPolicy: ProportionalLayout new.
	self layoutChanged
]

{ #category : '*Morphic-Widgets' }
Morph >> collapse [
	CollapsedMorph new beReplacementFor: self
]

{ #category : '*Morphic-Widgets' }
Morph >> containingWindow [
	"Answer a window that contains the receiver"

	^ self ownerThatIsA: SystemWindow
]

{ #category : '*Morphic-Widgets' }
Morph >> defaultBalloonColor [

	^ BalloonMorph balloonColor
]

{ #category : '*Morphic-Widgets' }
Morph >> defaultBalloonFont [
	^ BalloonMorph balloonFont
]

{ #category : '*Morphic-Widgets' }
Morph >> defaultBitmapFillForm [
	^ImageMorph defaultForm
]

{ #category : '*Morphic-Widgets' }
Morph >> defaultLabel [
	"Answer the default label to be used"
	^ self printString truncateTo: 40
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> detachAllKeymapCategories [

	self kmDispatcher detachAllKeymapCategories
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> detachKeymapCategory: aCategoryName [

	self kmDispatcher detachKeymapCategory: aCategoryName
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> detachKeymapCategory: aCategoryName targetting: anObject [

	self kmDispatcher detachKeymapCategory: aCategoryName targetting: anObject
]

{ #category : '*Morphic-Widgets' }
Morph >> dialogWindow [
	"Answer the receiver's dialog window."

	^self ownerThatIsA: DialogWindowMorph
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> dispatchKeystrokeForEvent: evt [
	self kmDispatcher dispatchKeystroke: evt
]

{ #category : '*Morphic-Widgets' }
Morph >> embedInWindow [

	| window worldToUse |

	worldToUse := self world.		"I'm assuming we are already in a world"
	window := (SystemWindow labelled: self defaultLabel) model: nil.
	window bounds: ((self position - ((0@window labelHeight) + window borderWidth))
						corner: self bottomRight + window borderWidth).
	window addMorph: self frame: (0@0 extent: 1@1).
	window updatePaneColors.
	worldToUse addMorph: window.
	window activate
]

{ #category : '*Morphic-Widgets' }
Morph >> embeddedInMorphicWindowLabeled: labelString [
	| window |
	window := (SystemWindow labelled: labelString) model: nil.
	window setStripeColorsFrom:  (self theme windowColorFor: self).
	window addMorph: self frame: (0@0 extent: 1@1).
	^ window
]

{ #category : '*Morphic-Widgets' }
Morph >> exportAs: anExtension using: aWriter [

	| reference |
	reference := self morphicUIManager
		chooseForSaveFileReference: 'Save Morph as ', anExtension asUppercase
		extensions: { anExtension }
		path: (self externalName, '.' , anExtension).
	reference ifNotNil: [ aWriter putForm: self imageForm onFileNamed: reference ]
]

{ #category : '*Morphic-Widgets' }
Morph >> exportAsBMP [

	self exportAs: 'bmp' using: BMPReadWriter
]

{ #category : '*Morphic-Widgets' }
Morph >> exportAsGIF [

	self exportAs: 'gif' using: GIFReadWriter
]

{ #category : '*Morphic-Widgets' }
Morph >> exportAsJPEG [
	"Export the receiver's image as a JPEG"

	self exportAs: 'jpeg' using: JPEGReadWriter
]

{ #category : '*Morphic-Widgets' }
Morph >> exportAsPNG [

	self exportAs: 'png' using: PNGReadWriter
]

{ #category : '*Morphic-Widgets' }
Morph >> fillWithRamp: rampSpecsOrColor oriented: aRatio [
	rampSpecsOrColor isColor
		ifTrue: [self color: rampSpecsOrColor".
			self borderColor: rampSpecsOrColor muchDarker"]
		ifFalse: [| fill |
			fill := GradientFillStyle ramp: rampSpecsOrColor.
			fill origin: self bounds topLeft.
			fill direction: (self bounds extent * aRatio) truncated.
			fill radial: false.
			self fillStyle: fill.
			self borderColor: (rampSpecsOrColor first value mixed: 0.5 with: rampSpecsOrColor last value) muchDarker]
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> handleKeystrokeWithKeymappings: aKeystrokeEvent [

	self allowsKeymapping ifTrue: [
		self dispatchKeystrokeForEvent: aKeystrokeEvent.
	]
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> hasKeymapCategoryNamed: aString [

	^ self kmDispatcher targets anySatisfy: [ :e | e category name = aString ]
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> initializeShortcuts: aKMDispatcher [
	"Where we may attach keymaps or even on:do: local shortcuts if needed."

	aKMDispatcher attachCategory: #MorphCtrlNavigation
]

{ #category : '*Morphic-Widgets' }
Morph >> isMenuItemMorph [
	^false
]

{ #category : '*Morphic-Widgets' }
Morph >> isMenuLineMorph [
	^false
]

{ #category : '*Morphic-Widgets' }
Morph >> isMenuMorph [
	^false
]

{ #category : '*Morphic-Widgets' }
Morph >> isWindowActive: aSystemWindow [
	"Answer whether the given window is active.
	Implement in morphs that are designed to embed system windows.
	Delegate to owner."

	^self owner ifNil: [true] ifNotNil: [:o | o isWindowActive: aSystemWindow]
]

{ #category : '*Morphic-Widgets' }
Morph >> justDroppedInto: aMorph event: anEvent [
	"This message is sent to a dropped morph after it has been dropped on -- and been accepted by -- a drop-sensitive morph"

	| aWindow |
	self formerOwner: nil.
	self formerPosition: nil.
	(aWindow := aMorph ownerThatIsA: SystemWindow) ifNotNil: [ aWindow isActive ifFalse: [ aWindow activate ] ].

	"An object launched by certain parts-launcher mechanisms should end up fully visible..."
	(self hasProperty: #beFullyVisibleAfterDrop) ifTrue: [
		aMorph == self world ifTrue: [ self goHome ].
		self removeProperty: #beFullyVisibleAfterDrop ]
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> kmDispatcher [
	"When creating the KMDispatcher instance, load default shortcuts."

	^ self
		valueOfProperty: #kmDispatcher
		ifAbsentPut: [
			| kmd |
			kmd := KMDispatcher target: self.
			self initializeShortcuts: kmd.
			kmd ]
]

{ #category : '*Morphic-Widgets' }
Morph >> morphicUIManager [

	^ MorphicUIManager new
]

{ #category : '*Morphic-Widgets' }
Morph >> on: eventName send: selector to: recipient [
	self eventHandler ifNil: [self eventHandler: MorphicEventHandler new].
	self eventHandler on: eventName send: selector to: recipient
]

{ #category : '*Morphic-Widgets' }
Morph >> on: eventName send: selector to: recipient withValue: value [
	"NOTE: selector must take 3 arguments, of which value will be the *** FIRST ***"

	self eventHandler ifNil: [self eventHandler: MorphicEventHandler new].
	self eventHandler on: eventName send: selector to: recipient withValue: value
]

{ #category : '*Morphic-Widgets' }
Morph >> openInWindow [

	^self openInWindowLabeled: self defaultLabel
]

{ #category : '*Morphic-Widgets' }
Morph >> openInWindowLabeled: aString [

	^self openInWindowLabeled: aString inWorld: self currentWorld
]

{ #category : '*Morphic-Widgets' }
Morph >> openInWindowLabeled: aString inWorld: aWorld [
	"Changed to include the inset margin in the bound calculation."

	^ self wrappedInWindow
		setLabel: aString;
		openInWorld: aWorld
]

{ #category : '*Morphic-Widgets-keymapping' }
Morph >> removeKeyCombination: aShortcut [

	self kmDispatcher removeKeyCombination: aShortcut
]

{ #category : '*Morphic-Widgets' }
Morph class >> systemIconName [

	^ #morph
]

{ #category : '*Morphic-Widgets' }
Morph class >> theme [
	"Answer the ui theme that provides controls."

	^ Smalltalk ui theme
]

{ #category : '*Morphic-Widgets' }
Morph >> theme [
	"Answer the current theme for the receiver.
	Compatibility: plain morphs are not styled; widgets get their theme from TMorphicWidget. Senders should move to widgets before this goes away."

	(self valueOfProperty: #theme) ifNotNil: [:t | ^ t].
	^(self owner ifNil: [self class]) theme
]

{ #category : '*Morphic-Widgets' }
Morph >> theme: aUITheme [
	"Set the current theme for the receiver and propagate theme changed to submorphs."

	self theme = aUITheme ifFalse: [
		self setProperty: #theme toValue: aUITheme.
		"we do not invoke basicTheme: to avoid checking twice if the theme changed or not"
		self themeChanged]
]

{ #category : '*Morphic-Widgets' }
Morph >> tileBottom [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max leftCenter corner: max corner)
]

{ #category : '*Morphic-Widgets' }
Morph >> tileBottomLeft [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max leftCenter corner: max bottomCenter)
]

{ #category : '*Morphic-Widgets' }
Morph >> tileBottomRight [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max center corner: max corner)
]

{ #category : '*Morphic-Widgets' }
Morph >> tileCenter [

	| max initial |
	max := RealEstateAgent maximumUsableArea.
	initial := self initialExtent.
	self bounds: ((max center - (initial/2)) extent: initial)
]

{ #category : '*Morphic-Widgets' }
Morph >> tileFull [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: max
]

{ #category : '*Morphic-Widgets' }
Morph >> tileLeft [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topLeft corner: max bottomCenter)
]

{ #category : '*Morphic-Widgets' }
Morph >> tileRight [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topCenter corner: max corner)
]

{ #category : '*Morphic-Widgets' }
Morph >> tileTop [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topLeft corner: max rightCenter)
]

{ #category : '*Morphic-Widgets' }
Morph >> tileTopLeft [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topLeft corner: max center)
]

{ #category : '*Morphic-Widgets' }
Morph >> tileTopRight [

	| max |
	max := RealEstateAgent maximumUsableArea.
	self bounds: (max topCenter corner: max rightCenter)
]

{ #category : '*Morphic-Widgets' }
Morph >> useBitmapFill [
	"Make receiver use a solid fill style (e.g., a simple color)"
	| fill |
	self fillStyle isBitmapFill ifTrue:[^self]. "Already done"
	fill := BitmapFillStyle fromForm: self defaultBitmapFillForm.
	"Note: Must fix the origin due to global coordinates"
	fill origin: self bounds origin.
	self fillStyle: fill
]

{ #category : '*Morphic-Widgets' }
Morph >> useGradientFill [
	"Make receiver use a solid fill style (e.g., a simple color)"
	| fill color1 color2 |
	self fillStyle isGradientFill ifTrue:[^self]. "Already done"
	color1 := self color asColor.
	color2 := color1 negated.
	fill := GradientFillStyle ramp: {0.0 -> color1. 1.0 -> color2}.
	fill origin: self topLeft.
	fill direction: 0 @ self bounds extent y.
	fill normal: self bounds extent x @ 0.
	fill radial: false.
	self fillStyle: fill
]

{ #category : '*Morphic-Widgets' }
Morph >> window [
	"Answer the receiver's window."

	^self ownerThatIsA: SystemWindow
]

{ #category : '*Morphic-Widgets' }
Morph >> wrappedInWindow [
	"Changed to include the inset margin in the bound calculation."

	| window extent |
	window := SystemWindow new.
	window
		addMorph: self frame: (0@0 extent: 1@1);
		updatePaneColors.
	" calculate extent after adding in case any size related attributes were changed.  Use
	fullBounds in order to trigger re-layout of layout morphs"
	extent := self fullBounds extent +
			(window borderWidth@window labelHeight) + window borderWidth +
			(window class borderWidth * 2 @ (window class borderWidth + 1)). "include inset margin"
	window extent: extent.
	^ window
]

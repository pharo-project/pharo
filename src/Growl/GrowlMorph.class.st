"
A GrowlMorph is a little Morph to announce event happening. Freely inspired from the MIT Snarl developed by  Tony Garnock-Jones. 

GrowlMorph new openInWorld.

10 timesRepeat: [
	(GrowlMorph openWithLabel: 'The time' contents: DateAndTime now)
""		vanishDelay: 1000;
		resetVanishTimer"".
	World doOneCycle ].

(GrowlMorph openWithLabel: 'The time' contents: DateAndTime now) 
	actionBlock: [Transcript open].
"
Class {
	#name : #GrowlMorph,
	#superclass : #TextMorph,
	#instVars : [
		'dismissHandle',
		'vanishTime',
		'alpha',
		'actionBlock',
		'vanishDelay',
		'labelAttr',
		'contentsAttr',
		'labelColor',
		'contentsColor'
	],
	#classVars : [
		'DefaultBackgroundColor',
		'Position',
		'Type'
	],
	#category : #'Growl-Base'
}

{ #category : #'instance creation' }
GrowlMorph class >> contents: contentString [
	
	^ self label: '' contents: contentString
]

{ #category : #settings }
GrowlMorph class >> growlPositionChoices [

	^#(topRight 'top right' 
		bottomLeft 'bottom left'
		bottomRight 'bottom right'
		topLeft 'topLeft') pairsCollect: [:a :b | b -> a]
]

{ #category : #settings }
GrowlMorph class >> growlSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #growl)
		label: 'Popup notification';  
		parent: #appearance;
		description: 'All settings concerned with the notifications popup look''n feel';
		with: [
			(aBuilder pickOne: #type)
				label: 'Popup notifycation type';
				target: self;
				default: #growl;
				order: 1;
				domainValues: self growlTypeChoices.
			(aBuilder pickOne: #position)
				label: 'Popup position';
				target: self;
				default: #bottomLeft;
				order: 2;
				domainValues: self growlPositionChoices.]
]

{ #category : #settings }
GrowlMorph class >> growlTypeChoices [

	^#(growl 'Growl Morph' 
		window 'Dialog window') pairsCollect: [:a :b | b -> a]
]

{ #category : #'instance creation' }
GrowlMorph class >> label: aString contents: contentString [
	
	^ self new
		label: aString contents: contentString;
		yourself
]

{ #category : #'instance creation' }
GrowlMorph class >> openWithContents: contentString [

	^ self openWithLabel: '' 
		contents: contentString 
		backgroundColor: nil 
		labelColor: nil
]

{ #category : #'instance creation' }
GrowlMorph class >> openWithLabel: aString contents: contentString [

	^ self openWithLabel: aString 
		contents: contentString 
		backgroundColor: nil 
		labelColor: nil
]

{ #category : #'instance creation' }
GrowlMorph class >> openWithLabel: aString contents: contentString backgroundColor: aColor labelColor: aLabelColor [
	
	^ self type = #window 
		ifTrue: [ MessageDialogWindow new message: contentString ]
		ifFalse: [ 
			| anInstance |
			anInstance := self label: aString contents: contentString.
			aColor ifNotNil: [ anInstance backgroundColor: aColor ].
			aLabelColor ifNotNil: [ 
				anInstance labelColor: aLabelColor.
				anInstance contentsColor: aLabelColor ].
			anInstance openInWorld ]
]

{ #category : #'instance creation' }
GrowlMorph class >> openWithLabel: aString contents: contentString color: aColor [
	
	^ self openWithLabel: aString 
		contents: contentString 
		backgroundColor: aColor 
		labelColor: nil
]

{ #category : #position }
GrowlMorph class >> position [

	^ Position ifNil: [ Position := #topRight ]
]

{ #category : #position }
GrowlMorph class >> position: aSymbol [

	(self possiblePositions includes: aSymbol) ifFalse: [ ^ self ].

	Position := aSymbol
]

{ #category : #position }
GrowlMorph class >> possiblePositions [

	^ #( bottomRight bottomLeft topRight topLeft )
]

{ #category : #position }
GrowlMorph class >> possibleTypes [

	^ #( growl window )
]

{ #category : #position }
GrowlMorph class >> type [

	^ Type ifNil: [ Type := #growl ]
]

{ #category : #position }
GrowlMorph class >> type: aSymbol [

	(self possibleTypes includes: aSymbol) ifFalse: [ ^ self ].

	Type := aSymbol
]

{ #category : #building }
GrowlMorph >> actionBlock: aBlock [

	actionBlock := aBlock
]

{ #category : #internal }
GrowlMorph >> activeGrowlMorphs [

	^self currentWorld submorphs select: [ :morph | morph isKindOf: GrowlMorph ].
]

{ #category : #accessing }
GrowlMorph >> alpha [
	^ alpha

]

{ #category : #accessing }
GrowlMorph >> alpha: newAlpha [

	alpha := newAlpha.
	labelAttr color: (self labelColor alpha: alpha).
	contentsAttr color: (self contentsColor alpha: alpha).
	self backgroundColor: (self nextColorStep: self backgroundColor).
	self allMorphsDo: [:m |
		m borderColor: (self nextColorStep: m borderColor).
		m color: (self nextColorStep: m color)].
	self borderColor isTransparent ifTrue: [self delete].
]

{ #category : #accessing }
GrowlMorph >> backgroundColor [

	^ backgroundColor ifNil: [ backgroundColor := self defaultBackgroundColor ]
]

{ #category : #building }
GrowlMorph >> contents: contentsString [

	self streamDo: [ :w |
		w withAttributes: self contentsAttributes do: [w nextPutAll: contentsString asString]]
]

{ #category : #initialization }
GrowlMorph >> contentsAttributes [
	^ { contentsAttr. TextAlignment centered. TextFontChange font2. }
]

{ #category : #accessing }
GrowlMorph >> contentsColor [
	^ contentsColor ifNil: [ contentsColor := self theme growlContentsColorFor: self ]

]

{ #category : #accessing }
GrowlMorph >> contentsColor: aColor [
	"when you set this contentsColor, it takes precedence over theme one. In certain case (such as for green as in SUnit) it is needed, normally you do not need it."
	contentsColor := aColor.
	contentsAttr color: aColor
	
]

{ #category : #initialization }
GrowlMorph >> createDismissHandle [
	| handle |
	handle := self theme growlDismissHandleFor: self.
	handle on: #mouseUp send: #delete to: self.
	^ handle
]

{ #category : #default }
GrowlMorph >> defaultBackgroundColor [

	^ self theme growlFillColorFor: self
]

{ #category : #default }
GrowlMorph >> defaultBorderColor [
	
	^ self theme growlBorderColorFor: self
]

{ #category : #default }
GrowlMorph >> defaultTextStyle [
	^ TextStyle actualTextStyles at: #Accuny
]

{ #category : #default }
GrowlMorph >> defaultVanishDelay [
	
	^ 1  seconds
]

{ #category : #accessing }
GrowlMorph >> enabled [

	^ false
]

{ #category : #'event handling' }
GrowlMorph >> handlesMouseDown: evt [
	^ actionBlock notNil or: [super handlesMouseDown: evt]
]

{ #category : #initialization }
GrowlMorph >> initialize [
	super initialize.
 	self  borderStyle: (BorderStyle color: self defaultBorderColor width: 1).
	
	self setProperty: #autoFitContents toValue: false.
 
	self initializeLabelAttributes.
	self initializeContentsAttributes.
	self vanishDelay: self defaultVanishDelay.
	self label: 'A cool title' contents: 'Here an important message'.
	dismissHandle := self createDismissHandle.
	self addMorph: dismissHandle
]

{ #category : #initialization }
GrowlMorph >> initializeContentsAttributes [
	contentsAttr := TextColor color: self contentsColor.
	
]

{ #category : #initialization }
GrowlMorph >> initializeLabelAttributes [
	labelAttr := TextColor color: self labelColor.
	
]

{ #category : #internal }
GrowlMorph >> is: rect saneWithRespectTo: morphs [

	^(morphs anySatisfy: [ :morph | morph owner isNotNil and: [morph bounds intersects: rect]]) not

]

{ #category : #building }
GrowlMorph >> label: labelString contents: contentsString [

	self streamDo: [ :w |
		w withAttributes: self labelAttributes do: [w nextPutAll: labelString asString; cr].
		w withAttributes: self contentsAttributes do: [w nextPutAll: contentsString asString].
		].
]

{ #category : #initialization }
GrowlMorph >> labelAttributes [
	^ { labelAttr. TextAlignment centered. TextFontChange font4. TextEmphasis bold. }
]

{ #category : #accessing }
GrowlMorph >> labelColor [
	^ labelColor ifNil: [ labelColor := self theme growlLabelColorFor: self ]
]

{ #category : #accessing }
GrowlMorph >> labelColor: aColor [
	"when you set this labelColor, it takes precedence over theme one. In certain case (such as for green as in SUnit) it is needed, normally you do not need it."
	labelColor := aColor.
	labelAttr color: self labelColor.
]

{ #category : #default }
GrowlMorph >> minimumExtent [
	^ 256@38
]

{ #category : #'event handling' }
GrowlMorph >> mouseDown: evt [
	super mouseDown: evt.
	evt yellowButtonPressed ifTrue: [^ self].
	actionBlock ifNotNil: [actionBlock valueWithPossibleArgs: { self }].
]

{ #category : #initialization }
GrowlMorph >> nextColorStep: aColor [
	^ aColor alpha: self alpha
]

{ #category : #display }
GrowlMorph >> openInWorld [

	self position: self unoccupiedPosition.
	^super openInWorld
]

{ #category : #internal }
GrowlMorph >> resetAlpha [
	^ self alpha: 0.9
]

{ #category : #internal }
GrowlMorph >> resetVanishTimer [

	vanishTime := DateAndTime now + self vanishDelay.
	self resetAlpha.
]

{ #category : #stepping }
GrowlMorph >> step [

	(self containsPoint: ActiveHand position) ifTrue: [
		self resetAlpha.
		^ self].
	vanishTime ifNotNil: [DateAndTime now < vanishTime ifTrue: [^self]].
	self alpha: self alpha - 0.05.
]

{ #category : #stepping }
GrowlMorph >> stepTime [
	^ 100
]

{ #category : #internal }
GrowlMorph >> streamDo: aBlock [

	self contentsWrapped: (Text streamContents: aBlock).
	self extent: self minimumExtent.
	self height: (paragraph extent y + (self borderWidth * 2) + (margins ifNil: [0] ifNotNil: [margins top + margins bottom]) + 2).

	self vanishDelay: ((((self contents size /50)seconds)+1 seconds) max: self defaultVanishDelay).
]

{ #category : #position }
GrowlMorph >> unoccupiedPosition [

	self class position = #bottomLeft
		ifTrue: [ ^ self unoccupiedPositionBottomLeft ].
	self class position = #topRight
		ifTrue: [ ^ self unoccupiedPositionTopRight ].
	self class position = #bottomRight
		ifTrue: [ ^ self unoccupiedPositionBottomRight ].
	self class position = #topLeft
		ifTrue: [ ^ self unoccupiedPositionTopLeft ].
	^ 0@0	
	
]

{ #category : #position }
GrowlMorph >> unoccupiedPositionBottomLeft [

	| startPos area rect morphs |
	
	startPos := 10 negated @ (self height + 10).
	area := self currentWorld clearArea.
	rect := (area bottomLeft - startPos) extent: self extent.
	
	morphs := self activeGrowlMorphs.
	[self is: rect saneWithRespectTo: morphs] whileFalse: [
		rect := rect translateBy: 0@(-10).
		rect top < 0 ifTrue: [^ area bottomLeft - startPos]
	].
	^ rect origin
]

{ #category : #position }
GrowlMorph >> unoccupiedPositionBottomRight [

	| startPos area rect morphs |
	
	startPos := (self width + 10 ) @ (self height + 10).
	area := self currentWorld clearArea.
	rect := (area bottomRight - startPos) extent: self extent.
	
	morphs := self activeGrowlMorphs.	
	[self is: rect saneWithRespectTo: morphs] whileFalse: [
		rect := rect translateBy: 0@(-10).
		rect top < 0 ifTrue: [^ area bottomRight - startPos ]
	].
	^ rect origin
]

{ #category : #position }
GrowlMorph >> unoccupiedPositionTopLeft [

	| startPos area rect morphs |
	
	startPos := 10@10.
	area := self currentWorld clearArea.
	rect := area topLeft + (startPos) extent: self extent.
	
	morphs := self activeGrowlMorphs.
	[self is: rect saneWithRespectTo: morphs] whileFalse: [
		rect := rect translateBy: 0@10.
		rect bottom > area height ifTrue: [^ area topLeft + startPos ]
	].
	^ rect origin
]

{ #category : #position }
GrowlMorph >> unoccupiedPositionTopRight [

	| startPos area rect morphs |
	
	startPos := (self width + 10 @ 10 negated).
	area := self currentWorld clearArea.
	rect := (area topRight - startPos) extent: self extent.
	
	morphs := self activeGrowlMorphs.	
	[self is: rect saneWithRespectTo: morphs] whileFalse: [
		rect := rect translateBy: 0@10.
		rect bottom > area height ifTrue: [^ (area topRight - startPos) ]
	].

	^ rect origin
]

{ #category : #accessing }
GrowlMorph >> vanishDelay [

	^ vanishDelay
]

{ #category : #accessing }
GrowlMorph >> vanishDelay: aDuration [

	vanishDelay := aDuration.
	self resetVanishTimer
]

{ #category : #stepping }
GrowlMorph >> wantsSteps [
	^ true
]

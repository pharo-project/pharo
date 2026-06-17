Class {
	#name : 'MissionControlMorph',
	#superclass : 'Morph',
	#instVars : [
		'thumbnails',
		'spacesBar',
		'isActive'
	],
	#category : 'Morphic-Widgets-MissionControl',
	#package : 'Morphic-Widgets-MissionControl'
}

{ #category : 'activation' }
MissionControlMorph class >> ensureMissionControlInWorld: world [

	world submorphs detect: [ :m | m isKindOf: self ] ifNone: [ self new activateInWorld: world ]
]

{ #category : 'world menu' }
MissionControlMorph class >> worldMenuEntryOn: aBuilder [

	<worldMenu>
	(aBuilder item: #'Mission Control')
		parent: #Windows;
		action: [ MissionControlMorph ensureMissionControlInWorld: self currentWorld ];
		help: 'Show all windows arranged in a single layer for easy navigation.';
		iconName: #window
]

{ #category : 'activation' }
MissionControlMorph >> activateInWorld: world [

	| nonCollapsed collapsed |

	nonCollapsed := world systemWindows reject: #isCollapsed.
	collapsed := world systemWindows select: #isCollapsed.

	self bounds: world bounds.
	spacesBar := MissionControlSpacesBar new initializeForWorld: world collapsedWindows: collapsed.
	
	self addMorph: spacesBar.
	world addMorphFront: self.
	self createThumbnailsFor: nonCollapsed.

	world activeHand newKeyboardFocus: self.
	isActive := true
]

{ #category : 'private' }
MissionControlMorph >> createThumbnailsFor: windows [

	| padding spacing available size cols rows cellW cellH labelHeight |

	windows ifEmpty: [ ^ self ].
	
	thumbnails := OrderedCollection new.
	
	size := windows size.
	
	padding := MissionControlSettings thumbnailPadding.
	available := self bounds insetBy: padding.
	available := available withTop: (available top + spacesBar height + padding).
	labelHeight := self labelHeight.
	cols := size sqrt ceiling.
	rows := (size / cols) ceiling.
	spacing := self spacing.
	
	cellW := (available width - (spacing * (cols - 1))) / cols.
	cellH := (available height - (labelHeight * rows) - (spacing * (rows - 1))) / rows.
	
	self createThumbnailsFor: windows cols: cols availableSpace: available cellW: cellW cellH: cellH labelHeight: labelHeight
]

{ #category : 'private' }
MissionControlMorph >> createThumbnailsFor: windows cols: cols availableSpace: available cellW: cellW cellH: cellH labelHeight: labelHeight [

	| thumbExtent spacing scale |
	
	spacing := self spacing.

	windows withIndexDo: [ :window :index | | col row thumbnail cellLeft cellTop |
		col := (index - 1) \\ cols.
		row := (index - 1) // cols.
		scale := (cellW / window extent x) min: (cellH / window extent y).
		scale := scale min: 1.0.
		thumbExtent := (window extent * scale) rounded.
		thumbnail := MissionControlThumbnailMorph new
			initializeForWindow: window extent: thumbExtent.
		self addMorph: thumbnail.
		thumbnails add: thumbnail.

		cellLeft := available left + (col * (cellW + spacing)).
		cellTop := available top + (row * (cellH + labelHeight + spacing)).
		thumbnail position: (self positionFor: thumbExtent cellW: cellW cellH: cellH cellLeft: cellLeft cellTop: cellTop).

		MissionControlSettings showWindowLabels ifTrue: [
			thumbnail buildLabel ifNotNil: [ :label |
				label position: (cellLeft + ((cellW - label width) // 2)) @ (thumbnail position y + thumbExtent y + 2).
				self addMorph: label
			]
		]
	]
]

{ #category : 'activation' }
MissionControlMorph >> deactivate [
	
	| world |
	
	world := self world.
	self delete.
	isActive := false.
	world ifNotNil: [ world activeHand newKeyboardFocus: nil ].
]

{ #category : 'activation' }
MissionControlMorph >> deactivateAndShowWindow: window [

	self deactivate.
	window unexpandedFrame: window fullFrame; activate
]

{ #category : 'drawing' }
MissionControlMorph >> drawOn: canvas [

	canvas
		fillRectangle: self bounds
		fillStyle: (SolidFillStyle color: (Color black alpha: MissionControlSettings overlayOpacity))
]

{ #category : 'event handling' }
MissionControlMorph >> handleMouseDown: event [
	
	| clicked |
	
	event wasHandled ifTrue: [ ^ self ].
	thumbnails ifNil: [ ^ self deactivate ].
	
	(spacesBar bounds containsPoint: event position) ifTrue: [ ^ self ].
	clicked := thumbnails detect: [ :thumbnail | thumbnail bounds containsPoint: event position ] ifNone: [ nil ].
	
	clicked ifNotNil: [ ^ self deactivateAndShowWindow: clicked window ].
	
	self deactivate
]

{ #category : 'event handling' }
MissionControlMorph >> handlesKeyDown: event [

	^ true
]

{ #category : 'event handling' }
MissionControlMorph >> handlesKeyboard: event [

	^ true
]

{ #category : 'event handling' }
MissionControlMorph >> handlesMouseDown: event [

	^ true
]

{ #category : 'accessing' }
MissionControlMorph >> isActive [

	^ isActive
]

{ #category : 'event handling' }
MissionControlMorph >> keyDown: event [

	event keyCharacter = Character escape ifTrue: [ ^ self deactivate ]
]

{ #category : 'private' }
MissionControlMorph >> labelHeight [

	^ MissionControlSettings showWindowLabels
		  ifTrue: [ StandardFonts defaultFont height + 4 ]
		  ifFalse: [ 0 ]
]

{ #category : 'wiw support' }
MissionControlMorph >> morphicLayerNumber [

	^ 5
]

{ #category : 'private' }
MissionControlMorph >> positionFor: thumbExtent cellW: cellW cellH: cellH cellLeft: cellLeft cellTop: cellTop [

	^ (cellLeft + ((cellW - thumbExtent x) // 2)) @ (cellTop + ((cellH - thumbExtent y) // 2))
]

{ #category : 'accessing' }
MissionControlMorph >> spacesBar [

	^ spacesBar
]

{ #category : 'private' }
MissionControlMorph >> spacing [

	"this defines the space between tabs"
	^ 10
]

{ #category : 'accessing' }
MissionControlMorph >> thumbnails [

	^ thumbnails
]

{ #category : 'accessing' }
MissionControlMorph >> wantsToBeTopmost [

	^ true
]

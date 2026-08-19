Class {
	#name : 'MissionControlSpacesBar',
	#superclass : 'BorderedMorph',
	#instVars : [
		'desktopThumbnail',
		'spaceThumbnails'
	],
	#category : 'Morphic-Widgets-MissionControl',
	#package : 'Morphic-Widgets-MissionControl'
}

{ #category : 'private' }
MissionControlSpacesBar >> buildDesktopThumbnailFor: world extent: thumbExtent [

	^ ImageMorph new
			form: (world asFormOfSize: thumbExtent);
			setProperty: #isDesktopThumbnail toValue: true;
			yourself
]

{ #category : 'private' }
MissionControlSpacesBar >> buildSpaceThumbnailFor: window extent: thumbExtent [

	^ (window taskThumbnailOfSize: thumbExtent)
		  setProperty: #window toValue: window;
		  yourself
]

{ #category : 'accessing' }
MissionControlSpacesBar >> desktopThumbnail [

	^ desktopThumbnail
]

{ #category : 'drawing' }
MissionControlSpacesBar >> drawOn: aCanvas [

	aCanvas fillRectangle: self bounds fillStyle: (SolidFillStyle color: (Color black alpha: 0.3)).
	super drawOn: aCanvas
]

{ #category : 'event handling' }
MissionControlSpacesBar >> handlesMouseDown: event [

	^ true
]

{ #category : 'initialization' }
MissionControlSpacesBar >> initializeForWorld: aWorld collapsedWindows: collapsedWindows [

	| barHeight thumbExtent |
	
	barHeight := 80.
	thumbExtent := 120 @ 60.

	self extent: aWorld width @ barHeight; position: aWorld position; color: (Color black alpha: 0.3); borderWidth: 0.
	self
		changeTableLayout;
		listDirection: #leftToRight;
		cellInset: 8;
		layoutInset: 8;
		hResizing: #spaceFill;
		vResizing: #rigid;
		wrapCentering: #center;
		cellPositioning: #center.
	desktopThumbnail := self buildDesktopThumbnailFor: aWorld extent: thumbExtent.
	
	self addMorph: desktopThumbnail.
	spaceThumbnails := OrderedCollection new.
	
	collapsedWindows do: [ :window | | thumb |
		thumb := self buildSpaceThumbnailFor: window extent: thumbExtent.
		self addMorph: thumb.
		spaceThumbnails add: thumb
	]
]

{ #category : 'event handling' }
MissionControlSpacesBar >> mouseDown: event [

	| clicked |
	
	clicked := self submorphs detect: [ :m | m bounds containsPoint: event position ] ifNone: [ nil ].
	clicked ifNil: [ ^ self ].
	
	(clicked hasProperty: #isDesktopThumbnail) ifTrue: [
		self owner ifNotNil: [ :mc | (mc isKindOf: MissionControlMorph) ifTrue: [ mc deactivate ] ].
		^ self
	].

	(clicked hasProperty: #window) ifTrue: [ | window |
		window := clicked valueOfProperty: #window.
		self owner ifNotNil: [ :mc | (mc isKindOf: MissionControlMorph) ifTrue: [ mc deactivateAndShowWindow: window ] ]
	]
]

{ #category : 'accessing' }
MissionControlSpacesBar >> spaceThumbnails [

	^ spaceThumbnails
]

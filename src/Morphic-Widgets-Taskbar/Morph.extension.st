Extension { #name : #Morph }

{ #category : #'*Morphic-Widgets-Taskbar' }
Morph >> defaultTaskbarThumbnailExtent [
	"Answer the default size of a taskbar thumbnail for the receiver."

	^320@320
]

{ #category : #'*Morphic-Widgets-Taskbar' }
Morph >> isTaskbar [
	"Answer false in the general case."

	^false
]

{ #category : #'*Morphic-Widgets-Taskbar' }
Morph >> taskThumbnailOfSize: thumbExtent [
	"Answer a new task thumbnail for the receiver."

	|f t r|
	r := self bounds scaledAndCenteredIn: (0@0 extent: thumbExtent).
	f := Form extent: r extent depth: Display depth.
	t := MatrixTransform2x3 withScale: f extent / self extent.
	f getCanvas
		transformBy: t
		clippingTo: f boundingBox
		during: [:c | c translateBy: self topLeft negated during: [:ct | self fullDrawOn: ct]]
		smoothing: 2.
	^ImageMorph new
		form: f
]

{ #category : #'*Morphic-Widgets-Taskbar' }
Morph >> taskbarButtonFor: aTaskBar [
	"Answer a new task bar button for the receiver.
	Answer nil if not required."

	^nil
]

{ #category : #'*Morphic-Widgets-Taskbar' }
Morph >> taskbarTask [
	"Answer a new taskbar task for the receiver.
	Answer nil if not required."

	^nil
]

{ #category : #'*Morphic-Widgets-Taskbar' }
Morph >> taskbarThumbnail [
	"Answer a new taskbar thumbnail for the receiver."

	^self taskThumbnailOfSize: self taskbarThumbnailExtent
]

{ #category : #'*Morphic-Widgets-Taskbar' }
Morph >> taskbarThumbnailExtent [
	"Answer the size of a taskbar thumbnail for the receiver."

	^self extent min: self defaultTaskbarThumbnailExtent
]

{ #category : #'*Morphic-Widgets-Taskbar' }
Morph >> taskbars [
	"Answer the receiver's taskbars."
	
	^self submorphs select: [:each |
		each isTaskbar]
]

Extension { #name : 'Screenshot' }

{ #category : '*Morphic-Base' }
Screenshot >> askRectangleFromUser [
	"Answer an instance of Rectangle that is determined by having the user
	designate the top left and bottom right corners. The gridding for user
	selection is 1@1."

	^ self askRectangleFromUserWithGrid: 1 @ 1
]

{ #category : '*Morphic-Base' }
Screenshot >> askRectangleFromUserWithGrid: gridPoint [

	^ self askRectangleFromUserWithGrid: gridPoint inWorld: self currentWorld
]

{ #category : '*Morphic-Base' }
Screenshot >> askRectangleFromUserWithGrid: gridPoint inWorld: aWorld [

	"Answer a Rectangle that is determined by having the user
	designate the top left and bottom right corners.
	The cursor reamins linked with the sensor, but
	the outline is kept gridded."

	| originRect |

	aWorld activeHand showTemporaryCursor: Cursor origin.

	originRect := ((aWorld activeHand cursorPoint grid: gridPoint) extent: 0 @ 0).

	originRect := self
		waitUserSelectionOriginRectangle: originRect
		transformBlock: [ :f :pt | (pt grid: gridPoint) extent: 0 @ 0 ]
		inWorld: aWorld.

	aWorld activeHand showTemporaryCursor: Cursor bottomRight.

	^ 	self
			waitUserSelectionOriginRectangle: originRect
			transformBlock: [ :f :pt | f origin corner: (pt grid: gridPoint) ]
			inWorld: aWorld
]

{ #category : '*Morphic-Base' }
Screenshot >> makeAScreenshot [

	| filePrefix |
	filePrefix := 'PharoScreenshot'.
	(MorphicUIManager new
		 chooseFrom: {
				 'The entire world' translated.
				 'A selected area' translated }
		 values: #( #world #area )
		 message:
			 'What do you want to shoot? (File will be saved in image directory)'
				 translated
		 title: 'Make a screenshot') ifNotNil: [ :choice |
		| form name |
		form := choice = #world
			        ifTrue: [ self currentWorld imageForm ]
			        ifFalse: [ Screenshot new formScreenshotFromUserSelection ].
		name := (FileSystem workingDirectory / filePrefix , 'png')
			        nextVersion.
		PNGReadWriter
			putForm: form
			onFileNamed:
			(FileSystem workingDirectory / filePrefix , 'png') nextVersion.
		MorphicUIManager new
			inform: (String streamContents: [ :s |
					 s
						 nextPutAll: 'Screenshot saved under ';
						 nextPutAll: name fullName;
						 cr;
						 nextPutAll: 'Click to open location' ])
			actionOnClick: [ name openInOSFileBrowser ] ]
]

{ #category : '*Morphic-Base' }
Screenshot >> waitUserSelectionOriginRectangle: originRect transformBlock: newRectBlock inWorld: aWorld [

	| hand buttonNow buttonStart rect selectionMorph newRect |
	hand := aWorld activeHand.

	buttonStart := buttonNow := hand anyButtonPressed.
	rect := originRect.

	selectionMorph := Morph new.
	selectionMorph color: Color transparent.
	selectionMorph borderColor: Color black.
	selectionMorph borderWidth: 2.
	selectionMorph openInWorld.
	selectionMorph bounds: originRect.

	hand captureEventsWhile: [ :evt |
		evt isMouse
			ifTrue: [
				buttonNow := evt anyButtonPressed.
				newRect := newRectBlock value: rect value: evt cursorPoint.

				newRect extent isZero ifFalse: [
					selectionMorph bounds: (newRect expandBy: 2) ].
				newRect = rect ifFalse: [ rect := newRect ].

				buttonNow = buttonStart ]
			ifFalse: [ true ] ].

	hand
		newMouseFocus: nil;
		showTemporaryCursor: nil.

	selectionMorph delete.

	^ rect
]

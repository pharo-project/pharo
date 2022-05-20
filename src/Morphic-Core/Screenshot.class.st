"
I implement the logic to do screenshots.
I can do a full screenshot or an area of the screen selected by the user.
I handle the user interaction to get the area to make the screenshot.

The method #makeAScreenshot handles the menu option to make screen shots.
"
Class {
	#name : #Screenshot,
	#superclass : #Object,
	#category : #'Morphic-Core'
}

{ #category : #'world menu items' }
Screenshot class >> screenShotCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Screenshot)
		parent: #System;
		target: self new;
		order: 99;
		selector: #makeAScreenshot;
		label: 'Screenshot';
		help: 'Take a screenshot';
		iconName: #smallScreenshotIcon
]

{ #category : #'user interface' }
Screenshot >> askRectangleFromUser [
	"Answer an instance of Rectangle that is determined by having the user 
	designate the top left and bottom right corners. The gridding for user 
	selection is 1@1."

	^ self askRectangleFromUserWithGrid: 1 @ 1
]

{ #category : #'user interface' }
Screenshot >> askRectangleFromUserWithGrid: gridPoint [ 

	^ self askRectangleFromUserWithGrid: gridPoint inWorld: self currentWorld
]

{ #category : #'user interface' }
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
			inWorld: aWorld.

]

{ #category : #operations }
Screenshot >> formScreenshotFromUserSelection [
	
	^ self formScreenshotFromUserSelection: 1 @ 1
]

{ #category : #operations }
Screenshot >> formScreenshotFromUserSelection: initialPoint [
	
	| rect |
	
	rect := self askRectangleFromUserWithGrid: initialPoint.
	^ Form fromDisplay: (rect origin extent: (rect extent max: initialPoint))
]

{ #category : #operations }
Screenshot >> makeAScreenshot [
	| filePrefix |
	filePrefix := 'PharoScreenshot'.
	(UIManager default
		chooseFrom: {'The entire world' translated . 'A selected area' translated}
		values: #(#world #area)
		message: 'What do you want to shoot? (File will be saved in image directory)' translated
		title: 'Make a screenshot')
		ifNotNil: [ :choice | 
			| form name |
			form := choice = #world ifTrue: [ self currentWorld imageForm ] ifFalse: [ Screenshot new formScreenshotFromUserSelection ].
			name := (FileSystem workingDirectory / filePrefix , 'png') nextVersion.
			PNGReadWriter putForm: form onFileNamed: (FileSystem workingDirectory / filePrefix , 'png') nextVersion.
			UIManager default
				inform:
					(String
						streamContents: [ :s | 
							s
								nextPutAll: 'Screenshot saved under ';
								nextPutAll: name fullName;
								cr;
								nextPutAll: 'Click to open location' ])
				actionOnClick: [ name openInOSFileBrowser ] ]
]

{ #category : #'user interface' }
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

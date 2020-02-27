"
Description
--------------------

I am a Form that is a possible appearance for a mouse cursor. My size is always 16x16, ever since the original implementation on the Alto.

There are many examples available in the ""constants"" category of class methods. For example, ""Cursor normal"" and ""Cursor wait"". 

Public API and Key Messages
--------------------

- #showWhile: 	When send to a cursor, it will use the cursor as the current cursor during the block execution

Examples
--------------------

	""Use the wait cursor for 3 seconds""
	Cursor wait showWhile: [ 3 second wait ].
	
	""Morph implements methods to help with the management of cursors via the hand morph. This is useful when you want to change the cursor and revert the change in different places instead of changinig it during the execution of a block.""
	self currentHand showTemporaryCursor: Cursor wait. 
	self flag: #doSomethingHere.
	self currentHand showTemporaryCursor: nil. ""Reset the cursor to the previous one""

"
Class {
	#name : #Cursor,
	#superclass : #Form,
	#classVars : [
		'BlankCursor',
		'BottomLeftCursor',
		'BottomRightCursor',
		'CornerCursor',
		'CrossHairCursor',
		'CurrentCursor',
		'DownCursor',
		'MarkerCursor',
		'MenuCursor',
		'MoveCursor',
		'NormalCursor',
		'OriginCursor',
		'OverEditableText',
		'ReadCursor',
		'ResizeLeftCursor',
		'ResizeTopCursor',
		'ResizeTopLeftCursor',
		'ResizeTopRightCursor',
		'RightArrowCursor',
		'SquareCursor',
		'TargetCursor',
		'TopLeftCursor',
		'TopRightCursor',
		'UpCursor',
		'WaitCursor',
		'WebLinkCursor',
		'WriteCursor',
		'XeqCursor'
	],
	#category : #'Graphics-Display Objects-Cursors'
}

{ #category : #constants }
Cursor class >> blank [
	"Answer the instance of me that is all white."

	^BlankCursor
]

{ #category : #constants }
Cursor class >> bottomLeft [
	"Cursor bottomLeft showWhile: [Sensor waitButton]"
	^BottomLeftCursor

]

{ #category : #constants }
Cursor class >> bottomRight [
	"Cursor bottomRight showWhile: [Sensor waitButton]"
	^BottomRightCursor

]

{ #category : #constants }
Cursor class >> corner [
	"Answer the instance of me that is the shape of the bottom right corner 
	of a rectangle."

	^CornerCursor
]

{ #category : #'initialize-release' }
Cursor class >> createStandardCursors [ 
	self 
		initOrigin;
		initRightArrow;
		initMenu;
		initCorner;
		initRead;
		initWrite;
		initWait.
	BlankCursor := Cursor new.
	self 
		initXeq;
		initSquare;
		initNormalWithMask;
		initCrossHair;
		initMarker;
		initUp;
		initDown;
		initMove;
		initBottomLeft;
		initBottomRight;
		initResizeLeft;
		initResizeTop;
		initResizeTopLeft;
		initResizeTopRight;
		initTopLeft;
		initTopRight;
		initTarget;
		initOverEditableText;
		makeCursorsWithMask.
]

{ #category : #constants }
Cursor class >> crossHair [
	"Answer the instance of me that is the shape of a cross."

	^CrossHairCursor
]

{ #category : #'current cursor' }
Cursor class >> currentCursor [
	"Answer the instance of Cursor that is the one currently displayed."

	^ ActiveHand ifNotNil: [ActiveHand currentCursor] ifNil: [ CurrentCursor ]
]

{ #category : #'current cursor' }
Cursor class >> currentCursor: aCursor [ 
	"Make the instance of cursor, aCursor, be the current cursor. Display it."

	ActiveHand ifNotNil: [
		ActiveHand currentCursor: aCursor.
		^ self ].

	CurrentCursor := aCursor	
]

{ #category : #constants }
Cursor class >> down [
	"Answer the instance of me that is the shape of an arrow facing 
	downward."

	^DownCursor
]

{ #category : #constants }
Cursor class >> execute [
	"Answer the instance of me that is the shape of an arrow slanted left 
	with a star next to it."

	^XeqCursor
]

{ #category : #'instance creation' }
Cursor class >> extent: extentPoint fromArray: anArray offset: offsetPoint [ 
	"Answer a new instance of me with width and height specified by
	extentPoint, offset by offsetPoint, and bits from anArray.
	NOTE: This has been kluged to take an array of 16-bit constants,
	and shift them over so they are left-justified in a 32-bit bitmap"

	extentPoint = (16 @ 16)
		ifTrue: 
			[^ super
				extent: extentPoint
				fromArray: (anArray collect: [:bits | bits bitShift: 16])
				offset: offsetPoint]
		ifFalse: [self error: 'cursors must be 16@16']
]

{ #category : #'initialization - private' }
Cursor class >> initBottomLeft [

	BottomLeftCursor := 
		(Cursor extent: 16 @ 16
			fromArray: #(
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1111111111111111
		2r1111111111111111)
			offset: 0 @ -16).

]

{ #category : #'initialization - private' }
Cursor class >> initBottomRight [

	BottomRightCursor := 
		(Cursor extent: 16 @ 16
			fromArray: #(
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r1111111111111111
		2r1111111111111111)
			offset: -16 @ -16).

]

{ #category : #'initialization - private' }
Cursor class >> initCorner [

	CornerCursor := 
		(Cursor 
			extent: 16 @ 16
			fromArray: #(
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r1111111111111111
		2r1111111111111111)
			offset: -16 @ -16).

]

{ #category : #'initialization - private' }
Cursor class >> initCrossHair [

	CrossHairCursor :=   
		(Cursor
			extent: 16 @ 16
			fromArray: #(
		2r0000000000000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0111111111111100
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000100000000
		2r0000000000000000
		2r0)
			offset: -7 @ -7).
	
	
]

{ #category : #'initialization - private' }
Cursor class >> initDown [

	DownCursor  :=
		     (Cursor
	extent: 16@16
	fromArray: #(
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r1111110000000000
		2r111100000000000
		2r11000000000000
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0)
	offset: 0@0).

]

{ #category : #'initialization - private' }
Cursor class >> initMarker [

	MarkerCursor := 
		Cursor
			extent: 16@16
			fromArray: #(
		2r0111000000000000
		2r1111100000000000
		2r1111100000000000
		2r0111000000000000
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0)
			offset: 0@0.

]

{ #category : #'initialization - private' }
Cursor class >> initMenu [ 

	MenuCursor  :=
		        (Cursor
	extent: 16@16
	fromArray: #(
		2r1111111111100000
		2r1000000000100000
		2r1010011000100000
		2r1000000000100000
		2r1101001101100000
		2r1111111111100000
		2r1000000000100000
		2r1011001010100000
		2r1000000000100000
		2r1010110010100000
		2r1000000000100000
		2r1010010100100000
		2r1000000000100000
		2r1111111111100000
		0)
	offset: 0@0).

]

{ #category : #'initialization - private' }
Cursor class >> initMove [

	MoveCursor := 
		Cursor 
			extent: 16@16
			fromArray: #(
		2r1111111111111100
		2r1111111111111100
		2r1100001100001100
		2r1100001100001100
		2r1100001100001100
		2r1100001100001100
		2r1111111111111100
		2r1111111111111100
		2r1100001100001100
		2r1100001100001100
		2r1100001100001100
		2r1100001100001100
		2r1111111111111100
		2r1111111111111100
          0)
			offset: 0@0.

]

{ #category : #'initialization - private' }
Cursor class >> initNormal [

	NormalCursor :=   
		(Cursor
			extent: 16@16
			fromArray: #(
		2r1000000000000000
		2r1100000000000000
		2r1110000000000000
		2r1111000000000000
		2r1111100000000000
		2r1111110000000000
		2r1111111000000000
		2r1111100000000000
		2r1111100000000000
		2r1001100000000000
		2r0000110000000000
		2r0000110000000000
		2r0000011000000000
		2r0000011000000000
		2r0000001100000000
		2r0000001100000000)
	offset: 0@0).

	
	
]

{ #category : #'initialization - private' }
Cursor class >> initNormalWithMask [    "Cursor initNormalWithMask.  Cursor normal show"
	"Next two lines work simply for any cursor..."
	self initNormal.
	NormalCursor := CursorWithMask derivedFrom: NormalCursor.

	"But for a good looking cursor, you have to tweak things..."
	NormalCursor := (CursorWithMask extent: 16 @ 16 depth: 1
			fromArray: #( 0 1073741824 1610612736 1879048192
				2013265920 2080374784 2113929216 2130706432
				2080374784 2080374784 1275068416 100663296
				100663296 50331648 50331648 0)
			offset: -1 @ -1)
		setMaskForm: (Form extent: 16@16 depth: 1
			fromArray: #( 3221225472 3758096384 4026531840 4160749568
				4227858432 4261412864 4278190080 4286578688
				4278190080 4261412864 4261412864 3472883712
				251658240 125829120 125829120 50331648)
			offset: 0 @ 0).
]

{ #category : #'initialization - private' }
Cursor class >> initOrigin [

	OriginCursor :=   
		(Cursor
			extent: 16@16
			fromArray: #(
		2r1111111111111111
		2r1111111111111111
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000)
			offset: 0@0).

]

{ #category : #'initialization - private' }
Cursor class >> initOverEditableText [
	OverEditableText := (Cursor extent: 16@16 fromArray: #(
				2r0
	               2r000111011100000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000000100000000
	               2r000111011100000
				2r0)
	       offset: (-7@ -8)) 


]

{ #category : #'initialization - private' }
Cursor class >> initRead [

	ReadCursor :=  
		(Cursor
			extent: 16@16
			fromArray: #(
		2r0000000000000000
		2r0000000000000000
		2r0001000000001000
		2r0010100000010100
		2r0100000000100000
		2r1111101111100000
		2r1000010000100000
		2r1000010000100000
		2r1011010110100000
		2r0111101111000000
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0)
	offset: 0@0).

]

{ #category : #'initialization - private' }
Cursor class >> initResizeLeft [

       ResizeLeftCursor :=
               (Cursor extent: 16 @ 16 fromArray: #(
               2r0000000000000000
               2r0000000000000000
               2r0000000000000000
               2r0000000000000000
               2r0000100000010000
               2r0001100000011000
               2r0011100000011100
               2r0111111111111110
               2r0011100000011100
               2r0001100000011000
               2r0000100000010000
               2r0000000000000000
               2r0000000000000000
               2r0000000000000000
               2r0000000000000000
               2r0000000000000000 )
       offset: -7 @ -7 ) withMask
]

{ #category : #'initialization - private' }
Cursor class >> initResizeTop [
    "Cursor initResizeTop"
       ResizeTopCursor :=
               (Cursor extent: 16 @ 16 fromArray: #(
               2r000000100000000
               2r000001110000000
               2r000011111000000
               2r000111111100000
               2r000000100000000
               2r000000100000000
               2r000000100000000
               2r000000100000000
               2r000000100000000
               2r000000100000000
               2r000111111100000
               2r000011111000000
               2r000001110000000
               2r000000100000000
               2r000000000000000)
       offset: -7 @ -7) withMask
]

{ #category : #'initialization - private' }
Cursor class >> initResizeTopLeft [

       ResizeTopLeftCursor :=
               (Cursor extent: 16 @ 16 fromArray: #(
               2r0000000000000000
               2r0111110000000000
               2r0111100000000000
               2r0111000000000000
               2r0110100000000000
               2r0100010000000000
               2r0000001000000000
               2r0000000100000000
               2r0000000010000000
               2r0000000001000100
               2r0000000000101100
               2r0000000000011100
               2r0000000000111100
               2r0000000001111100
               2r0000000000000000
               2r0000000000000000)
       offset: -7 @ -7) withMask
]

{ #category : #'initialization - private' }
Cursor class >> initResizeTopRight [

       ResizeTopRightCursor :=
               (Cursor extent: 16 @ 16 fromArray: #(
               2r0000000000000000
               2r0000000001111100
               2r0000000000111100
               2r0000000000011100
               2r0000000000101100
               2r0000000001000100
               2r0000000010000000
               2r0000000100000000
               2r0000001000000000
               2r0100010000000000
               2r0110100000000000
               2r0111000000000000
               2r0111100000000000
               2r0111110000000000
               2r0000000000000000
               2r0000000000000000)
       offset: -7 @ -7) withMask
]

{ #category : #'initialization - private' }
Cursor class >> initRightArrow [ 

	RightArrowCursor  :=
		      (Cursor
	extent: 16@16
	fromArray: #(
		2r100000000000
		2r111000000000
		2r1111111110000000
		2r111000000000
		2r100000000000
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0)
	offset: 0@0).
	
	"Cursor initRightArrow"
]

{ #category : #'initialization - private' }
Cursor class >> initSquare [

	SquareCursor := 
		(Cursor
			extent: 16 @ 16
			fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0000001111000000
		2r0000001111000000
		2r0000001111000000
		2r0000001111000000
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0)
	offset: -8 @ -8).

	
]

{ #category : #'initialization - private' }
Cursor class >> initTarget [
	^TargetCursor := Cursor
				extent: 16 @ 16
				fromArray:  #(1984 6448 8456 16644 17284 33026 35106 65278 35106 33026 17284 16644 8456 6448 1984 0)
				offset: -7 @ -7
]

{ #category : #'initialization - private' }
Cursor class >> initTopLeft [
	TopLeftCursor := 
		(Cursor extent: 16@16
			fromArray: #(
		2r1111111111111111
		2r1111111111111111
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000
		2r1100000000000000)
			offset: 0@0).

]

{ #category : #'initialization - private' }
Cursor class >> initTopRight [
	TopRightCursor := 
		(Cursor extent: 16@16
			fromArray: #(
		2r1111111111111111
		2r1111111111111111
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011
		2r0000000000000011)
			offset: -16@0).

]

{ #category : #'initialization - private' }
Cursor class >> initUp [

	UpCursor := 
		    (Cursor
	extent: 16@16
	fromArray: #(
		2r11000000000000
		2r111100000000000
		2r1111110000000000
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r11000000000000
		2r0
		2r0
		2r0
		2r0
		2r0
		2r0)
	offset: 0@0).

]

{ #category : #'initialization - private' }
Cursor class >> initWait [

	WaitCursor := 
		  (Cursor
			extent: 16@16
			fromArray: #(
		2r1111111111111100
		2r1000000000000100
		2r0100000000001000
		2r0010000000010000
		2r0001110011100000
		2r0000111111000000
		2r0000011110000000
		2r0000011110000000
		2r0000100101000000
		2r0001000100100000
		2r0010000110010000
		2r0100001111001000
		2r1000111111110100
		2r1111111111111100
		0)
			offset: 0@0).

]

{ #category : #'initialization - private' }
Cursor class >> initWrite [

	WriteCursor := (Cursor
	extent: 16@16
	fromArray: #(
		2r0000000000011000
		2r0000000000111100
		2r0000000001001000
		2r0000000010010000
		2r0000000100100000
		2r0000001001000100
		2r0000010010000100
		2r0000100100001100
		2r0001001000010000
		2r0010010000010000
		2r0111100000001000
		2r0101000011111000
		2r1110000110000000
		2r0111111100000000
		2r0
		2r0)
	offset: 0@0).

]

{ #category : #'initialization - private' }
Cursor class >> initXeq [

	XeqCursor := 
		(Cursor
			extent: 16@16
			fromArray: #(
		2r1000000000010000
		2r1100000000010000
		2r1110000000111000
		2r1111000111111111
		2r1111100011000110
		2r1111110001000100
		2r1111111001111100
		2r1111000001101100
		2r1101100011000110
		2r1001100010000010
		2r0000110000000000
		2r0000110000000000
		2r0000011000000000
		2r0000011000000000
		2r0000001100000000
		2r0000001100000000)
	offset: 0@0).

]

{ #category : #'initialize-release' }
Cursor class >> initialize [
	self createStandardCursors.
	SessionManager default registerGuiClassNamed: self name.
]

{ #category : #'initialization - private' }
Cursor class >> makeCursorsWithMask [
	"Cursor initialize;makeCursorsWithMask"

	self classPool associationsDo: [:var |
		var value hasMask
			ifFalse: [var value: var value withMask]] 
]

{ #category : #constants }
Cursor class >> marker [
	"Answer the instance of me that is the shape of a small ball."

	^MarkerCursor
]

{ #category : #constants }
Cursor class >> menu [ 
	"Answer the instance of me that is the shape of a menu."

	^MenuCursor
]

{ #category : #constants }
Cursor class >> move [
	"Answer the instance of me that is the shape of a cross inside a square."

	^MoveCursor
]

{ #category : #'instance creation' }
Cursor class >> new [

	^ self extent: 16 @ 16
		fromArray: (Array new: 16 withAll: 0)
		offset: 0 @ 0

]

{ #category : #constants }
Cursor class >> normal [
	"Answer the instance of me that is the shape of an arrow slanted left."

	^NormalCursor
]

{ #category : #constants }
Cursor class >> origin [
	"Answer the instance of me that is the shape of the top left corner of a 
	rectangle."

	^OriginCursor
]

{ #category : #constants }
Cursor class >> overEditableText [
	^ OverEditableText 
]

{ #category : #constants }
Cursor class >> read [
	"Answer the instance of me that is the shape of eyeglasses."

	^ReadCursor
]

{ #category : #constants }
Cursor class >> resizeBottom [
	"Cursor resizeBottom showWhile: [Sensor waitButton]"
	^self resizeTop
]

{ #category : #constants }
Cursor class >> resizeBottomLeft [
	"Cursor resizeBottomLeft showWhile: [Sensor waitButton]"
	^self resizeTopRight
]

{ #category : #constants }
Cursor class >> resizeBottomRight [
	"Cursor resizeBottomRight showWhile: [Sensor waitButton]"
	^self resizeTopLeft
]

{ #category : #'instance creation' }
Cursor class >> resizeForEdge: aSymbol [
	"Cursor resizeForEdge: #top"
	"Cursor resizeForEdge: #bottomLeft"
	^self perform: ('resize', aSymbol first asString asUppercase, (aSymbol copyFrom: 2 to: aSymbol size)) asSymbol.
]

{ #category : #constants }
Cursor class >> resizeLeft [
	"Cursor resizeLeft showWhile: [Sensor waitButton]"
	^ResizeLeftCursor
]

{ #category : #constants }
Cursor class >> resizeRight [
	"Cursor resizeRight showWhile: [Sensor waitButton]"
	^self resizeLeft
]

{ #category : #constants }
Cursor class >> resizeTop [
	"Cursor resizeTop showWhile: [Sensor waitButton]"
	^ResizeTopCursor
]

{ #category : #constants }
Cursor class >> resizeTopLeft [
	"Cursor resizeTopLeft showWhile: [Sensor waitButton]"
	^ ResizeTopLeftCursor
]

{ #category : #constants }
Cursor class >> resizeTopRight [
	"Cursor resizeTopRight showWhile: [Sensor waitButton]"
	^ResizeTopRightCursor
]

{ #category : #constants }
Cursor class >> rightArrow [ 
	"Answer the instance of me that is the shape of an arrow pointing to the right."

	^RightArrowCursor
]

{ #category : #constants }
Cursor class >> square [
	"Answer the instance of me that is the shape of a square."

	^SquareCursor
]

{ #category : #constants }
Cursor class >> target [
	"Answer the instance of me that is the shape of a gunsight."
	"Cursor target show"
	^TargetCursor ifNil:[self initTarget]
]

{ #category : #constants }
Cursor class >> topLeft [
	"Cursor topLeft showWhile: [Sensor waitButton]"
	^ TopLeftCursor
]

{ #category : #constants }
Cursor class >> topRight [
	"Cursor topRight showWhile: [Sensor waitButton]"
	^ TopRightCursor
]

{ #category : #constants }
Cursor class >> up [
	"Answer the instance of me that is the shape of an arrow facing upward."

	^UpCursor
]

{ #category : #constants }
Cursor class >> wait [
	"Answer the instance of me that is the shape of an Hourglass (was in the 
	shape of three small balls)."

	^WaitCursor
]

{ #category : #constants }
Cursor class >> webLink [
	"Return a cursor that can be used for emphasizing web links"
	"Cursor webLink showWhile: [Sensor waitButton]"
	^WebLinkCursor ifNil:[
		WebLinkCursor :=  (CursorWithMask extent: 16@16
			fromArray: #(3072 4608 4608 4608 4608 5046 4681 29257 37449 37449 32769 32769 49155 16386 24582 16380 )
			offset: -5@0) setMaskForm:
		(Form extent: 16@16 
			fromArray: (#(3072 7680 7680 7680 7680 8118 8191 32767 65535 65535 65535 65535 65535 32766 32766 16380 )  collect: [:bits | bits bitShift: 16])
			offset: 0@0)].
]

{ #category : #constants }
Cursor class >> write [
	"Answer the instance of me that is the shape of a pen writing."

	^WriteCursor
]

{ #category : #primitives }
Cursor >> activateInCursorOwner: aCursorOwner [
	^ self beCursor
]

{ #category : #converting }
Cursor >> asCursorForm [
	| form |
	form := Form extent: self extent depth: 8.
	form fillShape: self fillColor: Color black at: offset negated.
	^ form offset: offset
]

{ #category : #primitives }
Cursor >> beCursor [
	self primitiveBeCursor
]

{ #category : #primitives }
Cursor >> beCursorWithMask: maskForm [
	self primitiveBeCursorWithMask: maskForm
]

{ #category : #updating }
Cursor >> changed: aParameter [
	"overriden to reinstall the cursor if it is the active cursor, in case the appearance has changed.  (Is this used anywhere?  Do cursors really change in place these days?)"
	self flag: 'FIXME: Please, check on whether is this really needed'.
	
	"Removed for Morphic World specific cursor."
	"self == CurrentCursor ifTrue: [self beCursor]."
	super changed: aParameter
]

{ #category : #testing }
Cursor >> hasMask [
	^false
]

{ #category : #testing }
Cursor >> isCurrent [
	^ self class currentCursor == self 
]

{ #category : #primitives }
Cursor >> primitiveBeCursor [
	"Primitive. Tell the interpreter to use the receiver as the current cursor 
	image. Fail if the receiver does not match the size expected by the 
	hardware. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 101>
	self primitiveFailed
]

{ #category : #primitives }
Cursor >> primitiveBeCursorWithMask: maskForm [
	"Primitive. Tell the interpreter to use the receiver as the current cursor image with the given mask Form. Both the receiver and the mask should have extent 16@16 and a depth of one. The mask and cursor bits are combined as follow:
			mask	cursor	effect
			 0		  0		transparent (underlying pixel shows through)
			 1		  1		opaque black
			 1		  0		opaque white
			 0		  1		invert the underlying pixel"
"Essential. See Object documentation whatIsAPrimitive."

	<primitive: 101>
	self primitiveFailed

]

{ #category : #printing }
Cursor >> printOn: aStream [

	self storeOn: aStream base: 2
]

{ #category : #displaying }
Cursor >> show [
	"Make the hardware's mouse cursor look like the receiver"

	self class currentCursor: self
]

{ #category : #displaying }
Cursor >> showWhile: aBlock [ 
	"While evaluating the argument, aBlock, make the receiver be the cursor shape."
	| oldcursor |
	oldcursor := self class currentCursor.
	self show.
	^aBlock ensure: [oldcursor show]

]

{ #category : #converting }
Cursor >> withMask [
	^CursorWithMask derivedFrom: self
]

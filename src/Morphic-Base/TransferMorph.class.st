"
This is a Morph that is used to visually indicate the progress of a drag operation, and also as a container for various bits of drag state information.

It polls the shift state in its step method to update its copy state (shift pressed = should copy).

And if you hit the Escape key while dragging, it aborts the drag operation.
"
Class {
	#name : #TransferMorph,
	#superclass : #Morph,
	#instVars : [
		'transferType',
		'passenger',
		'draggedMorph',
		'source',
		'dropNotifyRecipient',
		'accepted',
		'copy',
		'dragHand'
	],
	#classVars : [
		'CopyPlusIcon'
	],
	#category : #'Morphic-Base-Events'
}

{ #category : #initialization }
TransferMorph class >> initIcons [
	"TransferMorph initIcons"

	CopyPlusIcon := Form
		extent: 16@16
		depth: 8
		fromArray: #( 0 0 65535 0 0 0 16768220 4278190080 0 0 16768220 4278190080 0 255 4294958300 4294967040 0 65500 3705461980 3705462015 0 65500 3705461980 3705462015 0 255 4294958300 4294967295 0 0 16768220 4278190080 0 0 16768220 4278190080 0 0 65535 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
		offset: 0@0
]

{ #category : #initialization }
TransferMorph class >> initialize [
	"TransferMorph initialize"

	self initIcons
]

{ #category : #'instance creation' }
TransferMorph class >> withPassenger: anObject [ 
	^ self withPassenger: anObject from: nil
]

{ #category : #'instance creation' }
TransferMorph class >> withPassenger: anObject from: source [ 
	^self withPassenger: anObject from: source hand: nil
]

{ #category : #'instance creation' }
TransferMorph class >> withPassenger: anObject from: source hand: dragHand [
	| hand |
	
	hand := (dragHand ifNil:[ ActiveHand ]).
	
	^ self new
		passenger: anObject;
		source: source;
		"If the client hasn't provided a hand use the currently active hand"
		dragHand: hand;
		shouldCopy: hand shiftPressed;
		position: source positionInWorld;
		yourself
]

{ #category : #'dropping/grabbing' }
TransferMorph >> aboutToBeGrabbedBy: aHand [ 
	"The receiver is being grabbed by a hand.                           
	Perform necessary adjustments (if any) and return the actual morph    
	     that should be added to the hand."
	"Since this morph has been initialized automatically with bounds origin   
	     0@0, we have to move it to aHand position."
	super aboutToBeGrabbedBy: aHand.
	self draggedMorph.
	"self align: self bottomLeft with: aHand position."
	self formerPosition: self position.
	aHand newKeyboardFocus: self.
]

{ #category : #'dropping/grabbing' }
TransferMorph >> animationForMoveSuccess: success [ 
	| start stop slideForm |
	success
		ifTrue: [^ self]
		ifFalse: [
			start := self fullBounds origin.
			stop := self formerPosition ifNil: [ self source bounds origin ]].
	start = stop ifTrue: [^ self].
	
	slideForm := self imageForm offset: 0@0.
	slideForm
		slideWithFirstFrom: start
		to: stop
		nSteps: 12
		delay: 20
]

{ #category : #initialization }
TransferMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ self theme selectionColor
]

{ #category : #'submorphs-add/remove' }
TransferMorph >> delete [
	"See also >>justDroppedInto:event:."
	accepted ifFalse: [
		self dropNotifyRecipient ifNotNil: [ self dropNotifyRecipient dropRejectedMorph: self ]].
	self changed: #deleted.
	self breakDependents.
	super delete
]

{ #category : #accessing }
TransferMorph >> dragHand [
	"The hand dragging this morph"
	^dragHand
]

{ #category : #accessing }
TransferMorph >> dragHand: aHandMorph [
	"The hand dragging this morph"
	dragHand := aHandMorph
]

{ #category : #'drag and drop' }
TransferMorph >> dragTransferType [
	^transferType
]

{ #category : #accessing }
TransferMorph >> dragTransferType: aSymbol [
	transferType := aSymbol
]

{ #category : #accessing }
TransferMorph >> draggedMorph [
	draggedMorph ifNil: [self initDraggedMorph].
	^draggedMorph
]

{ #category : #accessing }
TransferMorph >> draggedMorph: aMorph [
	draggedMorph := aMorph
]

{ #category : #accessing }
TransferMorph >> dropNotifyRecipient [
	^dropNotifyRecipient
]

{ #category : #accessing }
TransferMorph >> dropNotifyRecipient: anObject [
	dropNotifyRecipient := anObject
]

{ #category : #private }
TransferMorph >> initDraggedMorph [
	draggedMorph ifNotNil: [^self].
	draggedMorph := self passenger asDraggableMorph.
	self addMorphBack: draggedMorph.
	self updateCopyIcon.
	self changed; fullBounds
]

{ #category : #initialization }
TransferMorph >> initialize [
	"initialize the state of the receiver"
	super initialize.
	self layoutPolicy: TableLayout new.
	
	self listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		layoutInset: 3;
		cellInset: 3;
		wrapCentering: #center;
		cellPositioning: #leftCenter.
		
	self borderStyle: (BorderStyle
		width: 1 color: Color lightGray).
		
	accepted := false.	
	copy := false.
	self on: #keyStroke send: #keyStroke: to: self
]

{ #category : #testing }
TransferMorph >> isTransferable [

	^ true
]

{ #category : #'dropping/grabbing' }
TransferMorph >> justDroppedInto: targetMorph event: anEvent [ 
	"If only world wants this TransferMorph, treat it as unaccepted (see also >>delete)."
	| formerPosition |
	formerPosition := self formerPosition.
	super justDroppedInto: targetMorph event: anEvent.
	self formerPosition: formerPosition.
	accepted := targetMorph ~= self world.
	self animationForMoveSuccess: accepted.
	accepted ifTrue: [
		self dropNotifyRecipient ifNotNil: [
			self dropNotifyRecipient dropAcceptedMorph: self from: targetMorph ]].
	self delete
]

{ #category : #'event handling' }
TransferMorph >> keyStroke: evt [
	"Abort the drag on an escape"
	evt keyCharacter ~= Character escape ifTrue: [ ^self ].
	self delete.
]

{ #category : #accessing }
TransferMorph >> move [
	copy := false
]

{ #category : #accessing }
TransferMorph >> passenger [
	^passenger
]

{ #category : #accessing }
TransferMorph >> passenger: anObject [
	passenger := anObject
]

{ #category : #private }
TransferMorph >> privateFullMoveBy: delta [ 
	super privateFullMoveBy: delta.
	self changed: #position
]

{ #category : #'dropping/grabbing' }
TransferMorph >> result: result [ 
	^ self result: result from: nil
]

{ #category : #'dropping/grabbing' }
TransferMorph >> result: aResult from: aResultGenerator [ 
	"Send aResult of the drop operation computed by aResultGenerator to a   
	resultRecipient, if it exists."
]

{ #category : #accessing }
TransferMorph >> shouldCopy [
	^copy
]

{ #category : #accessing }
TransferMorph >> shouldCopy: aBoolean [
	copy := aBoolean.
]

{ #category : #accessing }
TransferMorph >> source [
	^source
]

{ #category : #accessing }
TransferMorph >> source: anObject [
	source := anObject
]

{ #category : #stepping }
TransferMorph >> step [
	self shouldCopy = dragHand shiftPressed
		ifTrue: [ ^ self ].
	
	self shouldCopy: dragHand shiftPressed.
	self updateCopyIcon.
]

{ #category : #stepping }
TransferMorph >> stepTime [
	^100
]

{ #category : #private }
TransferMorph >> updateCopyIcon [

	| copyIcon |	
	copyIcon := self submorphWithProperty: #tmCopyIcon.
	self shouldCopy ifFalse: [
		^ copyIcon ifNotNil: [ copyIcon delete ]].
	copyIcon ifNotNil: [ ^ self ].
	copyIcon := ImageMorph new form: CopyPlusIcon.
	copyIcon setProperty: #tmCopyIcon toValue: true.
	^ self addMorphFront: copyIcon.

]

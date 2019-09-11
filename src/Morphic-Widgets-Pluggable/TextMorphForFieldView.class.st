"
Single-line text field editor with DialogWindow key integration (return for default, escape for cancel) and keyboard focus navigation (tab/shift-tab).
"
Class {
	#name : #TextMorphForFieldView,
	#superclass : #TextMorphForEditView,
	#instVars : [
		'maxLength'
	],
	#category : #'Morphic-Widgets-Pluggable-Text'
}

{ #category : #private }
TextMorphForFieldView >> editorClass [
	^ TextEditor
]

{ #category : #testing }
TextMorphForFieldView >> isEditEvent: anEvent [
	"Answer whether the given event is a key stroke that
	should be allowed despite the max length."

	|key|
	key := anEvent keyValue asCharacter.
	^key == Character cr or: [
		key == Character escape or: [
		key == Character backspace or: [
		key == Character delete or: [
		key == Character arrowLeft or: [
		key == Character arrowRight or: [
		key == Character home or: [
		key == Character end or: [
			anEvent commandKeyPressed and: [key = $a]]]]]]]]]
]

{ #category : #'event handling' }
TextMorphForFieldView >> keyStroke: evt [
	"Don't allow editing keys if the edit view is disabled.
	Do the key if max length has not been reached.
	Don't allow tabs." 
	(self localHandleKeystroke: evt) ifTrue: [^self].
	(self maxLength isNil or: [self text size < self maxLength or: [
			self editor selectionInterval size > 0 or: [self isEditEvent: evt]]])
		ifTrue: [self doAutoAccept: evt].
	super keyStroke: evt
]

{ #category : #changed }
TextMorphForFieldView >> keyboardFocusChange: aBoolean [ 
	"If we are losing focus and have acceptOnFocusChange then accept."
	aBoolean
		ifTrue: [self editView selectAll]
		ifFalse: [self editView selectFrom: 1 to: 0].
	super keyboardFocusChange: aBoolean
]

{ #category : #events }
TextMorphForFieldView >> localHandleKeystroke: evt [
	"Answer whether we locally handle the keyStroke event.
	Disregard tabs for now."
	
	(self editView keystrokeFromTextMorph: evt)
		ifTrue: [^ true].
	(self autoAccept and: [evt keyCharacter = Character cr and: [self crAction notNil]])
		 ifTrue: [(self crAction isKindOf: MessageSend)
				ifTrue: [ self crAction value]
				ifFalse: [self crAction value: self text]..
			^ true].
	evt keyCharacter = Character tab ifTrue: [
		evt shiftPressed
			ifTrue: [(self editView respondsTo: #navigateFocusBackward)
						ifTrue: [self editView navigateFocusBackward]]
			ifFalse: [(self editView respondsTo: #navigateFocusForward)
						ifTrue: [self editView navigateFocusForward]].
		^true].
	^false
]

{ #category : #accessing }
TextMorphForFieldView >> maxLength [
	"Answer the value of maxLength"

	^ maxLength
]

{ #category : #accessing }
TextMorphForFieldView >> maxLength: anObject [
	"Set the value of maxLength"

	maxLength := anObject
]

{ #category : #accessing }
TextMorphForFieldView >> minExtent [

	"Overridden to allow text to shrink to minimum extent rather than being (incorrectly)
	treated as rigid. Should be the same for other morphs too. Bah!"

	| layout minExtent extra hFit vFit |

	hFit := self hResizing.
	vFit := self vResizing.
	( hFit == #spaceFill or: [ vFit == #spaceFill ] )
		ifFalse: [ "The receiver will not adjust to parents layout by growing or shrinking,
		which means that an accurate layout defines the minimum size." ^ self fullBounds extent ].
	layout := self layoutPolicy.
	minExtent := layout ifNil: [ 0 @ 0 ] ifNotNil: [ layout minExtentOf: self in: self layoutBounds ].
	minExtent := hFit == #rigid
		ifTrue: [ self fullBounds extent x @ minExtent y ]
		ifFalse: [ extra := self bounds width - self layoutBounds width.
			( minExtent x + extra ) @ minExtent y
			].
	minExtent := vFit == #rigid
		ifTrue: [ minExtent x @ self fullBounds extent y ]
		ifFalse: [ extra := self bounds height - self layoutBounds height.
			minExtent x @ ( minExtent y + extra )
			].
	minExtent := minExtent max: self minWidth @ self minHeight.
	^ minExtent
]

{ #category : #layout }
TextMorphForFieldView >> minHeight [
	"Answer the height of one line."

	^self minimumExtent y max: super minHeight
]

{ #category : #layout }
TextMorphForFieldView >> minWidth [
	"Answer the minimum width."

	^self minimumExtent x max: super minWidth
]

{ #category : #geometry }
TextMorphForFieldView >> minimumExtent [
	"Use the actual paragraph line to take font changes into account."
	
	| minExt |
	(textStyle isNil
		or: [ borderWidth isNil 
		or: [ self paragraph lines isEmpty ]])
		ifTrue: [ ^ 9@16 ].
		
	minExt := (9@(self paragraph lines first lineHeight ceiling)) + (borderWidth*2).
	^ ((0@0 extent: minExt) expandBy: margins) extent
]

{ #category : #'event handling' }
TextMorphForFieldView >> mouseMove: evt [
	"Allow auto scroll in any direction.
	Something else is preventing the left/right case."
	
	| editEvt |
	self perform: #mouseMove: withArguments: {evt} inSuperclass: TextMorph.
	evt redButtonPressed ifFalse: [^ self].
	editEvt := evt transformedBy: (self transformedFrom: editView) inverseTransformation.
	(editView bounds containsPoint: editEvt position) ifFalse:[
		"Start auto-scrolling"
		self startStepping: #autoScrollView:
			at: Time millisecondClockValue
			arguments: (Array with: editEvt)
			stepTime: 100. "fast enough"
	] ifTrue:[
		self stopSteppingSelector: #autoScrollView:.
	].
]

{ #category : #private }
TextMorphForFieldView >> paragraph [
	"Paragraph instantiation is lazy -- create it only when needed.
	Don't adjust rightX (as in TexMorph>>paragraph)
	since wrapFlag must be false for layout to work."
	
	paragraph ifNotNil: [^ paragraph].

self setProperty: #CreatingParagraph toValue: true.

	self setDefaultContentsIfNil.

	"...Code here to recreate the paragraph..."
	paragraph := (Paragraph new textOwner: self owner).
	paragraph wantsColumnBreaks: successor notNil.
	paragraph
		compose: text
		style: textStyle copy
		from: self startingIndex
		in: self container.
	"don't adjust the right, let it scroll!"
	paragraph focused: (self currentHand keyboardFocus == self).
	self fit.
self removeProperty: #CreatingParagraph.

	self selectionColor: self selectionColor.
	^ paragraph
]

{ #category : #'find-replace' }
TextMorphForFieldView >> sharesFindReplace [
	^ false
]

{ #category : #private }
TextMorphForFieldView >> useFindReplaceSelection [
	^ false
]

{ #category : #private }
TextMorphForFieldView >> useSecondarySelection [
	^ false
]

{ #category : #private }
TextMorphForFieldView >> useSelectionBar [
	^ false
]

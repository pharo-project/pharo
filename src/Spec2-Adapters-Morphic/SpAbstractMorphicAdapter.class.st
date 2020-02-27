"
I am an abstract class providing all the properties shared amongs all the morphic specific adapters
"
Class {
	#name : #SpAbstractMorphicAdapter,
	#superclass : #SpAbstractAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #accessing }
SpAbstractMorphicAdapter class >> adaptingName [
	"Override me if adapter will not follow the common pattern"

	"remove 'SpMorphic' part and *in general*, I will have the adapting name"

	^ (self name withoutPrefix: 'SpMorphic') asSymbol
]

{ #category : #accessing }
SpAbstractMorphicAdapter class >> allAdapters [

	^ self allSubclasses
]

{ #category : #'drag and drop' }
SpAbstractMorphicAdapter >> acceptDropBlock [

	^ self model acceptDropBlock
]

{ #category : #'drag and drop - private' }
SpAbstractMorphicAdapter >> acceptDroppingMorph: draggedMorph event: event inMorph: source [

	^ self acceptDropBlock cull: draggedMorph model model cull: event cull: source model model
]

{ #category : #initialization }
SpAbstractMorphicAdapter >> adapt: aPresenter [

	super adapt: aPresenter.
	self addSettingsTo: self widget.
	self addKeyBindingsTo: self widget.
	self flag: #TODO. "this should be here, but is interfering with old layouts reason why 
	 I'm moving it to MorphicLayoutAdapter >> #applyStyle:to:"
	"self applyStyle: self widget."
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> add: anAdapter [
	self widgetDo: [ :w | w spEnsureLayoutAndAddMorph: anAdapter widget ]
]

{ #category : #initialization }
SpAbstractMorphicAdapter >> addFocusRotationKeyBindings [

	self presenter canTakeKeyboardFocus ifFalse: [ ^ self ].

	self 
		bindKeyCombination: Character tab asKeyCombination 
		toAction: [ self focusNext ]. 
	self
		bindKeyCombination: Character tab shift asKeyCombination 
		toAction: [ self focusPrevious ]
]

{ #category : #initialization }
SpAbstractMorphicAdapter >> addKeyBindingsTo: aMorph [

	self addFocusRotationKeyBindings.
	self presenter contextKeyBindings ifNotNil: [ :aCategory | 
		aCategory allEntries keymaps do: [ :each |
			self 
				bindKeyCombination: each shortcut 
				toAction: each action ] ]
]

{ #category : #initialization }
SpAbstractMorphicAdapter >> addSettingsTo: aMorph [
	
	self widget ifNotNil: [ 
		self applyVisibility ]
]

{ #category : #accessing }
SpAbstractMorphicAdapter >> application [ 

	^ self presenter application
]

{ #category : #styling }
SpAbstractMorphicAdapter >> applyStyle: morph [
	"this will apply general properties to the built morph"
	
	self style applyTo: morph
]

{ #category : #initialization }
SpAbstractMorphicAdapter >> applyVisibility [
	
	"It may happen that some presenters use generic adapters and they do not have an associated presenter.
	In that case, do not manage visibility..."
	self presenter ifNil: [ ^ self ].

	self widget visible: self presenter isVisible.
	self presenter whenVisibleChangedDo: [ :visible |
		self presenter isVisible
			ifTrue: [ self widget show ]
			ifFalse: [ self widget hide ] ]
]

{ #category : #'protocol-shortcuts' }
SpAbstractMorphicAdapter >> bindKeyCombination: aShortcut toAction: aBlock [

	self widgetDo: [ :w | 
		w 
			bindKeyCombination: aShortcut 
			toAction: aBlock ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> borderColor [

	^ self model borderColor
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> borderColor: color [

	self widget ifNotNil: [ :w | w borderColor: color ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> borderWidth [

	^ self model borderWidth
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> borderWidth: width [

	self widget ifNotNil: [ :w | w borderWidth: width ]
]

{ #category : #'spec protocol' }
SpAbstractMorphicAdapter >> changed [

	^ self widgetDo: [ :w | w changed ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> color [

	^ self model color
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> color: color [

	self widgetDo: [ :w | w color: color ]
]

{ #category : #'spec protocol' }
SpAbstractMorphicAdapter >> delete [
	
	self widgetDo: [ :w | w delete ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> disable [
	
	self model enabled: false
]

{ #category : #'drag and drop' }
SpAbstractMorphicAdapter >> dragEnabled [

	^ self presenter dragEnabled
]

{ #category : #'drag and drop' }
SpAbstractMorphicAdapter >> dragEnabled: aBoolean [

	self widget ifNotNil: [ :w | w dragEnabled: aBoolean ]
]

{ #category : #'drag and drop - private' }
SpAbstractMorphicAdapter >> dragPassengerFor: item inMorph: dragSource [ 
	
	^ self dragTransformationBlock cull: item cull: dragSource
]

{ #category : #'drag and drop - private' }
SpAbstractMorphicAdapter >> dragPassengersFor: item inMorph: dragSource [ 
	
	^ { self dragTransformationBlock cull: item cull: dragSource }
]

{ #category : #'drag and drop' }
SpAbstractMorphicAdapter >> dragTransformationBlock [

	^ self model dragTransformationBlock
]

{ #category : #'drag and drop' }
SpAbstractMorphicAdapter >> dropEnabled [

	^ self presenter dropEnabled
]

{ #category : #'drag and drop' }
SpAbstractMorphicAdapter >> dropEnabled: aBoolean [

	self widget ifNotNil: [ :w | w dropEnabled: aBoolean ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> enable [

	self model enabled: true
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> enabled [
	^ self presenter isEnabled
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> enabled: aBoolean [

	self widget ifNotNil: [ :w | w enabled: aBoolean ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> extent [

	^ self widget extent
]

{ #category : #'focus handling' }
SpAbstractMorphicAdapter >> focusNext [
	
	self presenter withWindowDo: [ :window | | presenters |
		presenters := window allPresentersInFocusOrder.
		(presenters 
			after: self presenter
			ifAbsent: [ presenters first ])
			takeKeyboardFocus ]
]

{ #category : #'focus handling' }
SpAbstractMorphicAdapter >> focusPrevious [
	
	self presenter withWindowDo: [ :window | | presenters |
		presenters := window allPresentersInFocusOrder.
		(presenters 
			before: self presenter
			ifAbsent: [ presenters last ])
			takeKeyboardFocus ]
]

{ #category : #'as yet unclassified' }
SpAbstractMorphicAdapter >> gtInspectorPreviewIn: composite [
	<gtInspectorPresentationOrder: 30>
	
	self widgetDo: [ :w | w gtInspectorMorphIn: composite ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> hRigid [
	
	self widgetDo: [ :w | w hResizing: #rigid ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> hShrinkWrap [
	
	self widgetDo: [ :w | w hResizing: #shrinkWrap ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> hSpaceFill [
	
	self widgetDo: [ :w | w hResizing: #spaceFill ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> help [

	^ self model help
]

{ #category : #accessing }
SpAbstractMorphicAdapter >> helpText [
	
	^ self widget balloonText 
]

{ #category : #testing }
SpAbstractMorphicAdapter >> isMorphicAdapter [

	^ true
]

{ #category : #visibility }
SpAbstractMorphicAdapter >> isVisible [
	
	^ widget visible
]

{ #category : #emulating }
SpAbstractMorphicAdapter >> keyPressed: keyCode shift: shiftBoolean meta: metaBoolean control: controlBoolean option: optionBoolean [
	| evt modifiers |
	modifiers := 0.
	shiftBoolean ifTrue: [ modifiers := modifiers | 8 ].
	metaBoolean ifTrue: [ modifiers := modifiers |
		(Smalltalk os isWin32 | Smalltalk os isUnix
			ifTrue: [ 2r00010000 ]
			ifFalse: [ 2r01000000 ]) ].
	controlBoolean ifTrue: [ modifiers := modifiers | 2r00010000 ].
	optionBoolean ifTrue: [ modifiers := modifiers |
		(Smalltalk os isWin32 | Smalltalk os isUnix
			ifTrue: [ 2r01000000 ]
			ifFalse: [ 2r00100000 ]) ].
	
	evt := KeyboardEvent new
		setType: #keystroke
		buttons: modifiers
		position: self widget position + (1 @ 1)
		keyValue: keyCode
		charCode: keyCode
		hand: self currentWorld activeHand
		stamp: Time millisecondClockValue.

	evt sentTo: widget.
	"Some time to allow things to happen?"
	10 milliSeconds wait
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> layout: aLayout [
	| layout |
	layout := aLayout asMorphicLayout.
	self widgetDo: [ :w | w layoutFrame: layout ].
]

{ #category : #'protocol-shortcuts' }
SpAbstractMorphicAdapter >> removeKeyCombination: aShortcut [

	self widgetDo: [ :w | 
		w removeKeyCombination: aShortcut ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> removeSubWidgets [
	
	self widgetDo: [ :w | w removeAllMorphs ]
]

{ #category : #emulating }
SpAbstractMorphicAdapter >> rightClicked [
	| evt |

	evt := MouseButtonEvent new 
		setType: nil 
		position: widget center 
		which: MouseButtonEvent yellowButton 
		buttons: MouseButtonEvent yellowButton 
		hand: nil 
		stamp: nil.

	(widget handlesMouseDown: evt) ifTrue: [
		widget mouseDown: evt.
		widget mouseUp: evt ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> setBalloonText: aString [

	self widget ifNotNil: [ :w | w setBalloonText: aString ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> show [ 

	self widgetDo: #show
]

{ #category : #styling }
SpAbstractMorphicAdapter >> style [

	^ SpMorphStyle 
		fromStylesheet: self styleSheet 
		adapter: self
]

{ #category : #styling }
SpAbstractMorphicAdapter >> styleName [
	"i.e. SpButtonAdapter = button"
	
	^ ((self className withoutPrefix: 'SpMorphic') allButLast: 7) uncapitalized
]

{ #category : #styling }
SpAbstractMorphicAdapter >> styleSheet [

	^ self application styleSheet
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> takeKeyboardFocus [
	
	self widget ifNotNil: [ :w | w takeKeyboardFocus ]
]

{ #category : #'drag and drop' }
SpAbstractMorphicAdapter >> transferBlock [

	^ self model transferBlock
]

{ #category : #'drag and drop - private' }
SpAbstractMorphicAdapter >> transferFor: passenger from: source [
	^ (self transferBlock cull: passenger cull: self model) buildWithSpec
]

{ #category : #emulating }
SpAbstractMorphicAdapter >> type: aString [

	aString do: [ :each | 
		self 
			keyPressed: each asciiValue 
			shift: each isUppercase
			meta: false 
			control: false 
			option: false ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> useProportionalLayout [

	self widgetDo: [ :w | w changeProportionalLayout ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> vRigid [
	
	self widgetDo: [ :w | w vResizing: #rigid ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> vShrinkWrap [
	
	self widgetDo: [ :w | w vResizing: #shrinkWrap ]
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> vSpaceFill [
	
	self widgetDo: [ :w | w vResizing: #spaceFill ]
]

{ #category : #'drag and drop' }
SpAbstractMorphicAdapter >> wantDropBlock [

	^ self model wantDropBlock
]

{ #category : #'drag and drop - private' }
SpAbstractMorphicAdapter >> wantsDroppedMorph: draggedMorph event: anEvent inMorph: source [

	^ self wantDropBlock cull: draggedMorph cull: anEvent cull:source
]

{ #category : #protocol }
SpAbstractMorphicAdapter >> when: anAnnouncement do: aBlock [

	self widgetDo: [ :w | w announcer
		when: anAnnouncement do: aBlock ]
]

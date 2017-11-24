"
A simple button that handles multiple fillstyle states:
	Normal
	Mouse-over
	Mouse-down-inside
	Mouse-down outside	
with variants being a combination of passive/active and enabled/disabled.
"
Class {
	#name : #MultistateButtonMorph,
	#superclass : #Morph,
	#instVars : [
		'enabled',
		'active',
		'over',
		'down',
		'stateMap',
		'upAction'
	],
	#category : #'Morphic-Widgets-Basic-Buttons'
}

{ #category : #accessing }
MultistateButtonMorph >> active [
	"Answer the value of active"

	^ active
]

{ #category : #accessing }
MultistateButtonMorph >> active: anObject [
	"Set the value of active"

	active := anObject.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> activeDisabledNotOverDownFillStyle: aFillStyle [
	"Set the active, disabled, notOver, down fill style."

	self stateMap atPath: #(active disabled notOver down) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> activeDisabledNotOverUpFillStyle: aFillStyle [
	"Set the active, disabled, notOver, up fill style."

	self stateMap atPath: #(active disabled notOver up) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> activeDisabledOverDownFillStyle: aFillStyle [
	"Set the active, disabled, over, down fill style."

	self stateMap atPath: #(active disabled over down) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> activeDisabledOverUpFillStyle: aFillStyle [
	"Set the active, disabled, over, up fill style."

	self stateMap atPath: #(active disabled over up) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> activeEnabledNotOverDownFillStyle: aFillStyle [
	"Set the active, enabled, notOver, down fill style."

	self stateMap atPath: #(active enabled notOver down) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> activeEnabledNotOverUpFillStyle: aFillStyle [
	"Set the active, enabled, notOver, up fill style."

	self stateMap atPath: #(active enabled notOver up) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> activeEnabledOverDownFillStyle: aFillStyle [
	"Set the active, enabled, over, down fill style."

	self stateMap atPath: #(active enabled over down) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> activeEnabledOverUpFillStyle: aFillStyle [
	"Set the active, enabled, over, up fill style."

	self stateMap atPath: #(active enabled over up) put: aFillStyle.
	self changed
]

{ #category : #adding }
MultistateButtonMorph >> addUpAction: aValuable [
	
	upAction := aValuable
]

{ #category : #updating }
MultistateButtonMorph >> changed [
	"Update the fillStyle here."
	
	self assureExtension.
	extension fillStyle: self fillStyleToUse.
	color := self fillStyle asColor.
	super changed
]

{ #category : #accessing }
MultistateButtonMorph >> down [
	"Answer the value of down"

	^ down
]

{ #category : #accessing }
MultistateButtonMorph >> down: anObject [
	"Set the value of down"

	down := anObject.
	self changed
]

{ #category : #accessing }
MultistateButtonMorph >> enabled [
	"Answer whether the button is rnabled."

	^enabled
]

{ #category : #accessing }
MultistateButtonMorph >> enabled: anObject [
	"Set the value of enabled"

	enabled := anObject.
	self changed
]

{ #category : #geometry }
MultistateButtonMorph >> extent: aPoint [
	"Center the fill style origin."
	
	|delta|
	self bounds extent = aPoint ifTrue: [^self].
	delta := aPoint - self extent // 2.
	self fillStyles do: [:fs | fs isOrientedFill ifTrue: [fs origin: fs origin + delta]].
	super extent: aPoint
]

{ #category : #'visual properties' }
MultistateButtonMorph >> fillStyleToUse [
	"Answer the fill style to used based on the current state."

	|map|
	map := self active
		ifTrue: [self stateMap at: #active ifAbsent: [self stateMap at: #passive ifAbsent: [Dictionary new]]]
		ifFalse: [self stateMap at: #passive ifAbsent: [self stateMap at: #active ifAbsent: [Dictionary new]]].
	map := self enabled
		ifTrue: [map at: #enabled ifAbsent: [map at: #disabled ifAbsent: [Dictionary new]]]
		ifFalse: [map at: #disabled ifAbsent: [map at: #enabled ifAbsent: [Dictionary new]]].
	map := self over
		ifTrue: [map at: #over ifAbsent: [map at: #notOver ifAbsent: [Dictionary new]]]
		ifFalse: [map at: #notOver ifAbsent: [map at: #over ifAbsent: [Dictionary new]]].
	^map at: (self down ifTrue: [#down] ifFalse: [#up]) ifAbsent: [
		map at: (self down ifTrue: [#up] ifFalse: [#down]) ifAbsent: [Color transparent]]
]

{ #category : #'visual properties' }
MultistateButtonMorph >> fillStyles [
	"Answer all the fill styles"
	
	|styles|
	styles := OrderedCollection new.
	self stateMap do: [:actives |
		actives do: [:enableds |
			enableds do: [:overs |
				overs do: [:fs | styles add: fs]]]].
	^styles
]

{ #category : #'event handling' }
MultistateButtonMorph >> handlesMouseDown: evt [
	"Yes."
	
	^true
]

{ #category : #recategorized }
MultistateButtonMorph >> handlesMouseOver: anEvent [
	"Answer true, otherwise what is all that
	#mouseEnter:/#mouseLeave: stuff about?"

	^true
]

{ #category : #'event handling' }
MultistateButtonMorph >> handlesMouseOverDragging: evt [
	"Yes, for other states."
	
	^true
]

{ #category : #initialization }
MultistateButtonMorph >> initialize [
	"Initialize the receiver."

	self stateMap: KeyedTree new.
	enabled := true.
	active := true.
	over := false.
	down := false.
	super initialize
]

{ #category : #'visual properties' }
MultistateButtonMorph >> labelGraphic: anObject [
	"do nothing. this is a hack to make this multistate button work with a menu on a system window.
	Need to refactor menu boxes!"
]

{ #category : #'event handling' }
MultistateButtonMorph >> mouseDown: evt [ 
	"Handle a mouse down event."
	
	super mouseDown: evt.
	self enabled ifFalse: [^self].
	self down: true.
]

{ #category : #'event handling' }
MultistateButtonMorph >> mouseEnter: evt [
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed."

	super mouseEnter: evt.
	self over: true
]

{ #category : #'event handling' }
MultistateButtonMorph >> mouseEnterDragging: evt [
	"Handle a mouseEnterDragging event, meaning the mouse just entered my bounds with a button pressed or laden with submorphs."

	super mouseEnterDragging: evt.
	self over: true
]

{ #category : #'event handling' }
MultistateButtonMorph >> mouseLeave: evt [
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed."

	super mouseLeave: evt.
	self over: false
]

{ #category : #'event handling' }
MultistateButtonMorph >> mouseLeaveDragging: evt [
	"Handle a mouseLeaveLaden event, meaning the mouse just left my bounds with a button pressed or laden with submorphs."

	super mouseLeaveDragging: evt.
	self over: false
]

{ #category : #'event handling' }
MultistateButtonMorph >> mouseUp: evt [ 
	"Handle a mouse up event."
	
	super mouseUp: evt.
	self enabled ifFalse: [^self].
	self down: false.
	
	(self containsPoint: evt cursorPoint) ifTrue: [ 
		upAction value.
	]
]

{ #category : #accessing }
MultistateButtonMorph >> over [
	"Answer the value of over"

	^ over
]

{ #category : #accessing }
MultistateButtonMorph >> over: anObject [
	"Set the value of over"

	over := anObject.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> passiveDisabledNotOverDownFillStyle: aFillStyle [
	"Set the passive, disabled, notOver, down fill style."

	self stateMap atPath: #(passive disabled notOver down) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> passiveDisabledNotOverUpFillStyle: aFillStyle [
	"Set the passive, disabled, notOver, up fill style."

	self stateMap atPath: #(passive disabled notOver up) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> passiveDisabledOverDownFillStyle: aFillStyle [
	"Set the passive, disabled, over, down fill style."

	self stateMap atPath: #(passive disabled over down) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> passiveDisabledOverUpFillStyle: aFillStyle [
	"Set the passive, disabled, over, up fill style."

	self stateMap atPath: #(passive disabled over up) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> passiveEnabledNotOverDownFillStyle: aFillStyle [
	"Set the passive, enabled, notOver, down fill style."

	self stateMap atPath: #(passive enabled notOver down) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> passiveEnabledNotOverUpFillStyle: aFillStyle [
	"Set the passive, enabled, notOver, up fill style."

	self stateMap atPath: #(passive enabled notOver up) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> passiveEnabledOverDownFillStyle: aFillStyle [
	"Set the passive, enabled, over, down fill style."

	self stateMap atPath: #(passive enabled over down) put: aFillStyle.
	self changed
]

{ #category : #'visual properties' }
MultistateButtonMorph >> passiveEnabledOverUpFillStyle: aFillStyle [
	"Set the passive, enabled, over, up fill style."

	self stateMap atPath: #(passive enabled over up) put: aFillStyle.
	self changed
]

{ #category : #recategorized }
MultistateButtonMorph >> privateMoveBy: delta [
	"Adjust all the fill styles"
	
	super privateMoveBy: delta.
	(self fillStyles copyWithout: self fillStyle) do: [:fs | fs isOrientedFill ifTrue: [fs origin: fs origin + delta]]
]

{ #category : #accessing }
MultistateButtonMorph >> stateMap [
	"Answer the value of stateMap"

	^ stateMap
]

{ #category : #accessing }
MultistateButtonMorph >> stateMap: anObject [
	"Set the value of stateMap"

	stateMap := anObject
]

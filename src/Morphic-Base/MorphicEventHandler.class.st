"
I'm the class in charge of handling the subscriptions to events such as clicks or key presses.
I'm a provisory solution while my clients are fixed and I can be replaced by announcements
"
Class {
	#name : #MorphicEventHandler,
	#superclass : #Object,
	#instVars : [
		'subscriptions'
	],
	#category : #'Morphic-Base-Events'
}

{ #category : #events }
MorphicEventHandler >> addSubscription: aSubscription toEvent: eventName [
	(subscriptions includesKey: eventName)
		ifFalse: [ subscriptions at: eventName put: Set new. ].
	(subscriptions at: eventName)
		add: aSubscription.
]

{ #category : #access }
MorphicEventHandler >> allRecipients [
	^subscriptions collect: #recipients
]

{ #category : #events }
MorphicEventHandler >> click: event fromMorph: sourceMorph [ 
	^self notifyMorphsOfEvent: event ofType: #click from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> doubleClick: event fromMorph: sourceMorph [ 
	^self notifyMorphsOfEvent: event ofType: #doubleClick from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> doubleClickTimeout: event fromMorph: sourceMorph [ 
	^self notifyMorphsOfEvent: event ofType: #doubleClickTimeout from: sourceMorph
]

{ #category : #testing }
MorphicEventHandler >> existsSubscriptionsFor: anEvent [
	^(subscriptions includesKey: anEvent) and: [ (subscriptions at: anEvent) notEmpty ]
]

{ #category : #testing }
MorphicEventHandler >> handlesClickOrDrag: evt [
	^(self existsSubscriptionsFor: #click)
		or: [ (self existsSubscriptionsFor: #doubleClick) 
			or: [(self existsSubscriptionsFor: #startDrag)]].
]

{ #category : #testing }
MorphicEventHandler >> handlesGestureStart: evt [
	^self existsSubscriptionsFor: #gestureStart
]

{ #category : #testing }
MorphicEventHandler >> handlesKeyDown: evt [
	^self existsSubscriptionsFor: #keyDown
]

{ #category : #testing }
MorphicEventHandler >> handlesKeyStroke: evt [
	^self existsSubscriptionsFor: #keyStroke
]

{ #category : #testing }
MorphicEventHandler >> handlesKeyUp: evt [
	^self existsSubscriptionsFor: #keyUp
]

{ #category : #testing }
MorphicEventHandler >> handlesKeyboard: evt [
	^(self handlesKeyDown: evt)
		or: [ (self handlesKeyUp: evt)
			or: [ self handlesKeyStroke: evt ]].
]

{ #category : #testing }
MorphicEventHandler >> handlesMouseDown: evt [
	^(self existsSubscriptionsFor: #mouseDown)
		or: [ (self existsSubscriptionsFor: #mouseStillDown)
			or: [ (self existsSubscriptionsFor: #mouseUp)
				or: [ (self handlesClickOrDrag: evt)
					or: [ self handlesGestureStart: evt]]]].
]

{ #category : #testing }
MorphicEventHandler >> handlesMouseMove: evt [
	^self existsSubscriptionsFor: #mouseMove
]

{ #category : #testing }
MorphicEventHandler >> handlesMouseOver: evt [
	(self existsSubscriptionsFor: #mouseEnter) ifTrue: [ ^true ].
	(self existsSubscriptionsFor: #mouseLeave)  ifTrue: [ ^true ].
	(self existsSubscriptionsFor: #mouseOver) ifTrue: [ ^true ].
	^false.
		
]

{ #category : #testing }
MorphicEventHandler >> handlesMouseOverDragging: evt [
	^(self existsSubscriptionsFor: #mouseEnterDragging )
		or: [ self existsSubscriptionsFor: #mouseLeaveDragging ]
]

{ #category : #testing }
MorphicEventHandler >> handlesMouseStillDown: evt [
	^self existsSubscriptionsFor: #mouseStillDown.
]

{ #category : #initialization }
MorphicEventHandler >> initialize [
	subscriptions := Dictionary new.
]

{ #category : #events }
MorphicEventHandler >> keyDown: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #keyDown from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> keyStroke: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #keyStroke from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> keyUp: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #keyUp from: sourceMorph
]

{ #category : #access }
MorphicEventHandler >> methodRefList [
	"Return a MethodReference for each message I can send."
	| list |
	list := OrderedCollection new.
	subscriptions do: [
		:subscriptionSet |
		
		subscriptionSet do: [ 	 :s | 
			s  ifNotNil: [list
						add: (RGMethodDefinition
								realClass: (s recipient class whichClassIncludesSelector: s selector)
								selector: s selector)]
							]
		].
	^ list
]

{ #category : #events }
MorphicEventHandler >> mouseDown: event fromMorph: sourceMorph [ 
	"Take double-clicks into account."
	((self handlesClickOrDrag: event) and:[event redButtonPressed]) ifTrue:[
		event hand waitForClicksOrDrag: sourceMorph event: event.
	].
	^self notifyMorphsOfEvent: event ofType: #mouseDown from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> mouseEnter: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #mouseEnter from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> mouseEnterDragging: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #mouseEnterDragging from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> mouseLeave: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #mouseLeave from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> mouseLeaveDragging: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #mouseLeaveDragging from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> mouseMove: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #mouseMove from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> mouseOver: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #mouseOver from: sourceMorph
]

{ #category : #testing }
MorphicEventHandler >> mouseSelectorsInclude: selector [

	| mouseEventTypes allSubscriptions |
	mouseEventTypes := #(
		mouseDown
		mouseMove
		mouseStillDown
		mouseUp
		mouseEnter
		mouseLeave
		mouseEnterDragging
		mouseLeaveDragging
		doubleClick).
	
	allSubscriptions := subscriptions values flatCollect: [ :e | e ].
	^ allSubscriptions anySatisfy: [ :e | (mouseEventTypes includes: e event) and: [ e selector = selector ] ].
]

{ #category : #events }
MorphicEventHandler >> mouseStillDown: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #mouseStillDown from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> mouseUp: event fromMorph: sourceMorph [
	^self notifyMorphsOfEvent: event ofType: #mouseUp from: sourceMorph
]

{ #category : #events }
MorphicEventHandler >> notifyMorphsOfEvent: anEvent ofType: eventType from: sourceMorph [
	| result |
	result := false.
	((subscriptions includesKey: eventType) not or: [ (subscriptions at: eventType) isEmpty ])
		ifTrue: [ ^false ].
	(subscriptions at: eventType) do: [ :s | result := result | ((s notify: anEvent from: sourceMorph) == true) ].
	^result
]

{ #category : #initialization }
MorphicEventHandler >> on: eventName send: selector to: recipient [
	self
		addSubscription: (MorphEventSubscription on: eventName send: selector to: recipient)
		toEvent: eventName.
]

{ #category : #initialization }
MorphicEventHandler >> on: eventName send: selector to: recipient withValue: value [
	
	selector numArgs = 3 ifFalse:
		[self error: 'Warning: value parameters are passed as first of 3 arguments'].
	self
		addSubscription: (MorphEventSubscription on: eventName send: selector to: recipient withValue: value)
		toEvent: eventName.
	

]

{ #category : #events }
MorphicEventHandler >> startDrag: event fromMorph: sourceMorph [ 
	^self notifyMorphsOfEvent: event ofType: #startDrag from: sourceMorph
]

{ #category : #copying }
MorphicEventHandler >> veryDeepFixupWith: deepCopier [
	| old |
	"ALL inst vars were weakly copied.  If they were in the tree being copied, fix them up, otherwise point to the originals!!"

	super veryDeepFixupWith: deepCopier.
	1 to: self class instSize do: [:ii | 
		old := self instVarAt: ii.
		self instVarAt: ii put: (deepCopier references at: old ifAbsent: [old])
		].


]

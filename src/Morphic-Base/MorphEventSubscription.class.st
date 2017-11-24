"
I represent a subscription to an event.  I'm kind of an announcement, that's why I will be replaced by them soon :)
"
Class {
	#name : #MorphEventSubscription,
	#superclass : #Object,
	#instVars : [
		'event',
		'selector',
		'recipient',
		'valueParameter'
	],
	#category : #'Morphic-Base-Events'
}

{ #category : #'instance creation' }
MorphEventSubscription class >> on: anEvent send: aSelector to: anObject [
	^self new
		event: anEvent;
		selector: aSelector;
		recipient: anObject;
		yourself
]

{ #category : #'instance creation' }
MorphEventSubscription class >> on: anEvent send: aMessageSelector to: anObject withValue: aValue [
	^self new
		event: anEvent;
		selector: aMessageSelector;
		recipient: anObject;
		valueParameter: aValue;
		yourself
]

{ #category : #'events-triggering' }
MorphEventSubscription >> click: anEvent fromMorph: aMorph [ 
	^self notify: anEvent from: aMorph
]

{ #category : #'events-triggering' }
MorphEventSubscription >> doubleClick: anEvent fromMorph: aMorph [ 
	^self notify: anEvent from: aMorph
]

{ #category : #'events-triggering' }
MorphEventSubscription >> doubleClickTimeout: anEvent fromMorph: aMorph [ 
	^self notify: anEvent from: aMorph
]

{ #category : #accessing }
MorphEventSubscription >> event [

	^ event
]

{ #category : #accessing }
MorphEventSubscription >> event: anEvent [

	event := anEvent
]

{ #category : #'events-triggering' }
MorphEventSubscription >> mouseEnter: anEvent fromMorph: aMorph [ 
	^self notify: anEvent from: aMorph
]

{ #category : #'events-triggering' }
MorphEventSubscription >> mouseLeave: anEvent fromMorph: aMorph [ 
	^self notify: anEvent from: aMorph
]

{ #category : #'events-triggering' }
MorphEventSubscription >> notify: anEvent from: sourceMorph [
	| arity |
	recipient ifNil: [^ self].
	arity := selector numArgs.
	arity = 0 ifTrue:
		[^ recipient perform: selector].
	arity = 1 ifTrue:
		[^ recipient perform: selector with: anEvent].
	arity = 2 ifTrue:
		[^ recipient perform: selector with: anEvent with: sourceMorph].
	arity = 3 ifTrue:
		[^ recipient perform: selector with: valueParameter with: anEvent with: sourceMorph].
	self error: 'Event handling selectors must be Symbols and take 0-3 arguments'
]

{ #category : #accessing }
MorphEventSubscription >> recipient [

	^ recipient
]

{ #category : #accessing }
MorphEventSubscription >> recipient: anObject [

	recipient := anObject
]

{ #category : #accessing }
MorphEventSubscription >> selector [

	^ selector
]

{ #category : #accessing }
MorphEventSubscription >> selector: anObject [

	selector := anObject
]

{ #category : #accessing }
MorphEventSubscription >> valueParameter [

	^ valueParameter
]

{ #category : #accessing }
MorphEventSubscription >> valueParameter: anObject [

	valueParameter := anObject
]

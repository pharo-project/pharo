"
I am a window used by WindowSpec.

I have been introduced since it's currently the best solution to prevent to add dozen of respondsTo: in StandardWindow or add dirty extensions to Object (because all models are not subclasses of Model)
"
Class {
	#name : #SpWindow,
	#superclass : #StandardWindow,
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #protocol }
SpWindow >> aboutText [

	^ self model aboutText
]

{ #category : #closing }
SpWindow >> allowedToClose [

	super allowedToClose ifFalse: [ ^ false ].
	^ self model askOkToClose 
		ifTrue: [ self model requestWindowClose ]
		ifFalse: [ true ].
]

{ #category : #private }
SpWindow >> announceWillClose [
	| announcement |

	announcement := SpWindowWillClose new
		window: self;
		yourself.
	self announce: announcement.
	self currentWorld announcer announce: announcement
]

{ #category : #'open/close' }
SpWindow >> close [ 

	self announceWillClose.
	^ super close
]

{ #category : #'open/close' }
SpWindow >> deleteDiscardingChanges [

	self announceWillClose.
	^ super deleteDiscardingChanges
]

{ #category : #'open/close' }
SpWindow >> extent: aPoint [ 
	| announcement oldExtent |

	oldExtent := self extent. 
	super extent: aPoint.
	announcement := SpWindowResizing new
		window: self;
		oldSize: oldExtent;
		newSize: aPoint;
		yourself.
	self announce: announcement.
	self currentWorld announcer announce: announcement
]

{ #category : #testing }
SpWindow >> hasWidget: aMorph [ 
	
	^ self allMorphs includes: aMorph
]

{ #category : #updating }
SpWindow >> okToChange [

	^ self model okToChange
]

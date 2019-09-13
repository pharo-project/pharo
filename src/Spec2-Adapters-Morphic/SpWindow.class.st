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

{ #category : #'open/close' }
SpWindow >> deleteDiscardingChanges [
	| announcement |

	announcement := SpWindowWillClose new
		window: self;
		yourself.
	self announce: announcement.
	self currentWorld announcer announce: announcement.	
	^ super deleteDiscardingChanges
]

{ #category : #testing }
SpWindow >> hasWidget: aMorph [ 
	
	^ self allMorphs includes: aMorph
]

{ #category : #updating }
SpWindow >> okToChange [

	^ self model okToChange
]

"
I am a window used by WindowSpec.

I have been introduced since it's currently the best solution to prevent to add dozen of respondsTo: in StandardWindow or add dirty extensions to Object (because all models are not subclasses of Model)
"
Class {
	#name : #SpecWindow,
	#superclass : #StandardWindow,
	#category : #'Spec-MorphicAdapters-Support'
}

{ #category : #protocol }
SpecWindow >> aboutText [

	^ self model aboutText
]

{ #category : #controls }
SpecWindow >> close [

	self model askOkToClose
		ifTrue: [ self okToChange ifFalse: [ ^ self ] ].
		
	super close
]

{ #category : #updating }
SpecWindow >> okToChange [

	^ self model okToChange
]

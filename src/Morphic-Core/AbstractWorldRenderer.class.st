Class {
	#name : #AbstractWorldRenderer,
	#superclass : #Object,
	#instVars : [
		'world'
	],
	#category : #'Morphic-Core-Worlds'
}

{ #category : #accessing }
AbstractWorldRenderer class >> detectCorrectOneforWorld: aWorld [

(self allSubclasses sorted: [ :a :b | a priority > b priority ])
        detect: [ :aClass | aClass isApplicableFor: aWorld ]
        ifFound: [ :aClass | ^ aClass forWorld: aWorld ]
]

{ #category : #accessing }
AbstractWorldRenderer class >> forWorld: aWorld [

	^ self new 
		world: aWorld;
		yourself
]

{ #category : #accessing }
AbstractWorldRenderer class >> priority [ 

	^ 0
]

{ #category : #activation }
AbstractWorldRenderer >> activate [

	self subclassResponsibility 
]

{ #category : #activation }
AbstractWorldRenderer >> deactivate [

	self subclassResponsibility 
]

{ #category : #accessing }
AbstractWorldRenderer >> world [
	^ world
]

{ #category : #accessing }
AbstractWorldRenderer >> world: anObject [
	world := anObject
]

"
Please, do not use me. 

I serve for building of Menus (mostly in Morphic) without collecting pragmas which makes me faster but less flexible. End-user applications should use Spec and Commander2.
"
Class {
	#name : #BlockMenuBuilder,
	#superclass : #PragmaMenuBuilder,
	#instVars : [
		'buildingBlock'
	],
	#category : #'MenuRegistration-Core'
}

{ #category : #accessing }
BlockMenuBuilder >> buildingBlock [
	^ buildingBlock
]

{ #category : #accessing }
BlockMenuBuilder >> buildingBlock: anObject [
	buildingBlock := anObject
]

{ #category : #'registrations handling' }
BlockMenuBuilder >> collectRegistrations [

	self buildingBlock value: self
]

Class {
	#name : #GLMPhlowTextBrick,
	#superclass : #GLMPhlowBrick,
	#instVars : [
		'textBrick'
	],
	#category : #'Glamour-Morphic-Brick-Widgets-Phlow'
}

{ #category : #examples }
GLMPhlowTextBrick class >> example [
	
	^ GLMCompositeBrick gtExamplePharoMethod
]

{ #category : #phlow }
GLMPhlowTextBrick >> display: anObject [

	self setProperty: #phlowDisplay toValue: anObject
]

{ #category : #accessing }
GLMPhlowTextBrick >> displayLogic [

	^ self valueOfProperty: #phlowDisplay ifAbsent: [ #yourself ]
]

{ #category : #phlow }
GLMPhlowTextBrick >> entity: anObject [
	super entity: anObject.
	
	self installEntity
]

{ #category : #initialization }
GLMPhlowTextBrick >> initialize [
	super initialize.
	
	self textBrick: self newTextBrick.
]

{ #category : #phlow }
GLMPhlowTextBrick >> installEntity [
	self textBrick setTextWith: (self displayLogic phlowValue: self entity).
	self textBrick selectionInterval: (self selectionInterval phlowValue: self entity)
	
]

{ #category : #'instance creation' }
GLMPhlowTextBrick >> newTextBrick [

	^ GLMRubScrolledTextBrick new
]

{ #category : #accessing }
GLMPhlowTextBrick >> selectionInterval [
	
	^ self valueOfProperty: #phlowSelectionInterval ifAbsent: [ 1 to: 0 ]
]

{ #category : #accessing }
GLMPhlowTextBrick >> selectionInterval: anObject [
	
	self setProperty: #phlowSelectionInterval toValue: anObject
]

{ #category : #accessing }
GLMPhlowTextBrick >> textBrick [

	^ textBrick
	

]

{ #category : #accessing }
GLMPhlowTextBrick >> textBrick: aBrick [

	textBrick := aBrick.
	self textBrick
		wrapped: true.
		
	self textBrick layoutProperties
		vSpaceFill;
		hSpaceFill.
	self addBrickBack: self textBrick
	

]

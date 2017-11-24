Class {
	#name : #GLMPhlowBrick,
	#superclass : #GLMBrick,
	#instVars : [
		'entity',
		'titleBrick'
	],
	#category : #'Glamour-Morphic-Brick-Widgets-Phlow'
}

{ #category : #phlow }
GLMPhlowBrick >> display: aBlockOrSymbol [

	self subclassResponsibility
]

{ #category : #phlow }
GLMPhlowBrick >> entity [

	^ entity
]

{ #category : #phlow }
GLMPhlowBrick >> entity: anObject [

	entity := anObject.
	self setTitle: (self titleLogic phlowValue: self entity)
	
]

{ #category : #initialization }
GLMPhlowBrick >> initialize [
	super initialize.
		
	self
		useVerticalLinearLayout;
		titleBrick: self newTitleBrick;
		hSpaceFill;
		vSpaceFill
]

{ #category : #'instance creation' }
GLMPhlowBrick >> newTitleBrick [

	^ GLMLabelBrick new
]

{ #category : #'brick-properties-events' }
GLMPhlowBrick >> onThemerChanged [
	super onThemerChanged.
	
	self titleBrick textColor: self themer textLightColor
]

{ #category : #accessing }
GLMPhlowBrick >> setTitle: aString [

	self titleBrick text: aString.
	
	titleBrick height: (
		(aString isEmpty | aString isNil)
			ifFalse: [ 30 ]
			ifTrue: [ 0 ])
]

{ #category : #phlow }
GLMPhlowBrick >> title: anObject [
	
	self setProperty: #phlowTitle toValue: anObject
]

{ #category : #accessing }
GLMPhlowBrick >> titleBrick [
	^ titleBrick
]

{ #category : #accessing }
GLMPhlowBrick >> titleBrick: aBrick [

	titleBrick := aBrick
		textPosition: #center;
		height: 0;
		hSpaceFill.
	self addBrickBack: titleBrick
]

{ #category : #accessing }
GLMPhlowBrick >> titleLogic [

	^ self valueOfProperty: #phlowTitle ifAbsent: [ '' ]
]

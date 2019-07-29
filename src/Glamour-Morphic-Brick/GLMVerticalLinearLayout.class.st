Class {
	#name : #GLMVerticalLinearLayout,
	#superclass : #GLMLinearLayout,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Glamour-Morphic-Brick-Layouts'
}

{ #category : #cleanup }
GLMVerticalLinearLayout class >> cleanUp [

	self reset
]

{ #category : #initialization }
GLMVerticalLinearLayout class >> reset [
	"
	self reset
	"
	uniqueInstance := nil
]

{ #category : #accessing }
GLMVerticalLinearLayout class >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
]

{ #category : #layout }
GLMVerticalLinearLayout >> align: aBrick in: parentBrick anchorPoint: aPoint inContext: aContext [
	|align|
	
	self = aContext ifTrue: [ ^ self ].
	(parentBrick subbricks size = 1) & (aContext isNil) ifTrue: [
		GLMHorizontalLinearLayout uniqueInstance align: aBrick in: parentBrick anchorPoint: aPoint inContext: self ].
	
	align := aBrick layout hAlign.
	
	align = #none ifTrue: [ ^ self ].
	align = #left ifTrue: [ ^ self ].
	align = #center ifTrue: [
		^ aBrick translateX: ((parentBrick innerBounds width - aBrick outerBounds width) / 2) asInteger ].
	align = #right ifTrue: [
		^ aBrick translateX: ((parentBrick innerBounds width - aBrick outerBounds width)) asInteger ].
]

{ #category : #converting }
GLMVerticalLinearLayout >> asString [
	^ 'vertical'
]

{ #category : #height }
GLMVerticalLinearLayout >> heightChildrenDependency: aBrick [
	"returns the height of aBrick when it depends on children"
	^ aBrick paddingTop + aBrick paddingBottom + ((self nonFloatSubbricks: aBrick) detectSum: [ :each | each outerBounds height ])
]

{ #category : #width }
GLMVerticalLinearLayout >> heightParentDependency: aBrick [
	| allowedHeight |
	aBrick owner ifNil: [ Exception signal: 'My height depends on parent, but it is nil' ].
	allowedHeight := aBrick owner subbricks inject: aBrick owner innerBounds height into: [ :height :each |
		each isHeightClean ifTrue: [
			(each layout vState isOnParent not | (self hasHeightRestrictions: each))
				ifTrue: [ height - each outerBounds height ]
				ifFalse: [ height ] ]
			ifFalse: [ height ] ].
	^ (allowedHeight * aBrick layout vPercent / 100) ceiling
]

{ #category : #layout }
GLMVerticalLinearLayout >> layout: aBrick in: newBounds [

	self
		layout: aBrick
		in: newBounds
		inject: aBrick paddingLeft @ aBrick paddingTop
		into: [ :origin :each |
			each outerOrigin: origin.
			0@each outerBounds height ]
]

{ #category : #width }
GLMVerticalLinearLayout >> widthChildrenDependency: aBrick [
	"returns the height of aBrick when it depends on children"
	"in case of horizontal linear layout it is the height of the highest subbrick + corresponding paddings"
	^ aBrick paddingLeft + aBrick paddingRight + (((self nonFloatSubbricks: aBrick) detectMax: [ :each | each outerBounds width ])
		ifNil: [ 0 ]
		ifNotNil: [ :m | m outerBounds width ])
]

{ #category : #width }
GLMVerticalLinearLayout >> widthParentDependency: aBrick [
	
	aBrick owner ifNil: [ Exception signal: 'My width depends on parent, but it is nil' ].
	^ (aBrick owner innerBounds width * aBrick layout hPercent / 100) ceiling
]

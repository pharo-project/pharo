"
Use me in case something went wrong on production
"
Class {
	#name : #GLMFallbackScrollListBrick,
	#superclass : #GLMScrollPaneBrick,
	#instVars : [
		'adaptor',
		'emptyBrick'
	],
	#category : #Glamour-Morphic-Brick-Widgets-Scrollpane
}

{ #category : #accessing }
GLMFallbackScrollListBrick >> emptyBrick [

	^ emptyBrick
]

{ #category : #accessing }
GLMFallbackScrollListBrick >> emptyBrick: aBrick [
	
	emptyBrick := aBrick
]

{ #category : #initialization }
GLMFallbackScrollListBrick >> initializeItems [
	
	self band layoutAfter: [
		1 to: self model count do: [ :index |
			self band addBrickBack: ((self model morphAt: index convert: nil) asBrick with:[:b |
				b brickApi layout vState isOnParent ifTrue: [ b height: 30 ]]) ] ].
	
	self band hasSubbricks ifFalse: [ self emptyBrick ifNotNil: [ :b | self band addBrickBack: b ] ].
]

{ #category : #scrolling }
GLMFallbackScrollListBrick >> invalidate [

	self band removeAllBricks.
	self initializeItems.
	self doLayoutForce.
	self update.

]

{ #category : #accessing }
GLMFallbackScrollListBrick >> model [

	^ adaptor
]

{ #category : #accessing }
GLMFallbackScrollListBrick >> model: anAdaptor [

	adaptor := anAdaptor.
	self initializeItems
]

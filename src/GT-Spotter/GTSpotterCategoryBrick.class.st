Class {
	#name : #GTSpotterCategoryBrick,
	#superclass : #GLMInteractableBrick,
	#instVars : [
		'listBrick',
		'titleBrick',
		'stepModel',
		'category'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #adding }
GTSpotterCategoryBrick >> addItem: anItem [
	self addBrickBack: anItem
]

{ #category : #adding }
GTSpotterCategoryBrick >> addItems: aCollectionOfItems [
	self addAllBricks: aCollectionOfItems
]

{ #category : #accessing }
GTSpotterCategoryBrick >> category [

	^ category
]

{ #category : #accessing }
GTSpotterCategoryBrick >> category: aProcessorClass [

	category := aProcessorClass
]

{ #category : #initialization }
GTSpotterCategoryBrick >> initialize [
	super initialize.
	self
		useVerticalLinearLayout;
		vShrinkWrap;
		initializeListeners;
		enable
]

{ #category : #initialization }
GTSpotterCategoryBrick >> initializeListeners [
	self announcer when: #onMouseHover send: #onHovered to: self. 
	self announcer when: #onMouseUnhover send: #onUnhovered to: self
]

{ #category : #'instance creation' }
GTSpotterCategoryBrick >> on: aStepModel category: aProcessor [

	self stepModel: aStepModel.
	self category: aProcessor.
]

{ #category : #'event handling' }
GTSpotterCategoryBrick >> onHovered [
	self titleBrick showActionbar
]

{ #category : #'event handling' }
GTSpotterCategoryBrick >> onUnhovered [
	(self stepModel isCategorySelected: self category)
		ifFalse: [ self titleBrick hideActionbar ]
]

{ #category : #accessing }
GTSpotterCategoryBrick >> order [
	^ self category order
]

{ #category : #'event handling' }
GTSpotterCategoryBrick >> privateAddIntoWorld: aBrick [
	super privateAddIntoWorld: aBrick.
	self setAmountVisible: self subbricksCount all: '...'
]

{ #category : #accessing }
GTSpotterCategoryBrick >> setAmountVisible: visible all: all [

	self titleBrick setAmountVisible: visible all: all
]

{ #category : #accessing }
GTSpotterCategoryBrick >> stepModel [
	^ stepModel
]

{ #category : #accessing }
GTSpotterCategoryBrick >> stepModel: anObject [
	stepModel := anObject.
]

{ #category : #accessing }
GTSpotterCategoryBrick >> titleBrick [
	^ titleBrick
]

{ #category : #accessing }
GTSpotterCategoryBrick >> titleBrick: aBrick [

	titleBrick := aBrick
]

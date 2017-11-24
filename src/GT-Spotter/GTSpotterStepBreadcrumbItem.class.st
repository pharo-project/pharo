Class {
	#name : #GTSpotterStepBreadcrumbItem,
	#superclass : #GLMInteractableBrick,
	#instVars : [
		'labelBrick',
		'triangleBrick',
		'stepModel',
		'isTriangleVisible'
	],
	#category : #'GT-Spotter-UI'
}

{ #category : #actions }
GTSpotterStepBreadcrumbItem >> click: evt [
	super click: evt.
	
	self stepModel spotter popUntil: self stepModel.
	^ true
]

{ #category : #actions }
GTSpotterStepBreadcrumbItem >> deselect [
	
	self invalidateWith: [
		self labelBrick label customFont; beRegular ]
]

{ #category : #actions }
GTSpotterStepBreadcrumbItem >> hideTriangle [

	isTriangleVisible := false.
	self themer spotterThemer breadcrumbItemTriangleWidgetStyleFor: self triangleBrick
]

{ #category : #initialization }
GTSpotterStepBreadcrumbItem >> initialize [
	super initialize.
	
	self
		useVerticalLinearLayout;
		labelBrick: self newLabelBrick;
		triangleBrick: self newTriangleBrick;
		enable
	
]

{ #category : #testing }
GTSpotterStepBreadcrumbItem >> isTriangleVisible [

	^ isTriangleVisible ifNil: [ isTriangleVisible := true ]
]

{ #category : #accessing }
GTSpotterStepBreadcrumbItem >> labelBrick [

	^ labelBrick
]

{ #category : #accessing }
GTSpotterStepBreadcrumbItem >> labelBrick: aBrick [

	labelBrick := aBrick.
	
	self themer spotterThemer breadcrumbItemLabelStyleFor: self labelBrick.
	self addBrickBack: self labelBrick
]

{ #category : #'instance creation' }
GTSpotterStepBreadcrumbItem >> newLabelBrick [

	^ GLMLabelBrick new
]

{ #category : #'instance creation' }
GTSpotterStepBreadcrumbItem >> newTriangleBrick [

	^ GLMBrick new
]

{ #category : #'brick-properties-events' }
GTSpotterStepBreadcrumbItem >> onThemerChanged [
	super onThemerChanged.
	
	self themer spotterThemer breadcrumbItemLabelWidgetStyleFor: self labelBrick.
	self themer spotterThemer breadcrumbItemTriangleWidgetStyleFor: self triangleBrick
]

{ #category : #actions }
GTSpotterStepBreadcrumbItem >> select [
	
	self invalidateWith: [
		self labelBrick label customFont; beBold ]
]

{ #category : #actions }
GTSpotterStepBreadcrumbItem >> showTriangle [

	isTriangleVisible := true.
	self themer spotterThemer breadcrumbItemTriangleWidgetStyleFor: self triangleBrick
]

{ #category : #accessing }
GTSpotterStepBreadcrumbItem >> stepModel [
	^ stepModel
]

{ #category : #accessing }
GTSpotterStepBreadcrumbItem >> stepModel: anObject [
	stepModel := anObject
]

{ #category : #accessing }
GTSpotterStepBreadcrumbItem >> text: aString [

	self labelBrick text: aString.
	self doLayoutForce
]

{ #category : #accessing }
GTSpotterStepBreadcrumbItem >> triangleBrick [

	^ triangleBrick
]

{ #category : #accessing }
GTSpotterStepBreadcrumbItem >> triangleBrick: aBrick [

	triangleBrick := aBrick.
	
	self themer spotterThemer breadcrumbItemTriangleStyleFor: self triangleBrick.
	self addBrickBack: self triangleBrick
]

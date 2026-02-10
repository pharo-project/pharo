"
I provide a way of delivering form's bits to window's contents.
To obtain my instance, first one create a valid OSWindow and then use #newFormRenderer: .

Then you can render on form, and form's contents will serve as medium which will be displayed inside a window.
"
Class {
	#name : 'OSWindowFormRenderer',
	#superclass : 'OSWindowRenderer',
	#instVars : [
		'form'
	],
	#classInstVars : [
		'canvasScaleFactor',
		'autoSetCanvasScaleFactor'
	],
	#category : 'OSWindow-Core-Renderer',
	#package : 'OSWindow-Core',
	#tag : 'Renderer'
}

{ #category : 'accessing' }
OSWindowFormRenderer class >> autoSetCanvasScaleFactor [

	^ autoSetCanvasScaleFactor ifNil: [ autoSetCanvasScaleFactor := true ]
]

{ #category : 'accessing' }
OSWindowFormRenderer class >> autoSetCanvasScaleFactor: boolean [

	autoSetCanvasScaleFactor := boolean
]

{ #category : 'accessing' }
OSWindowFormRenderer class >> canvasScaleFactor [

	^ canvasScaleFactor ifNil: [ canvasScaleFactor := 1 ]
]

{ #category : 'accessing' }
OSWindowFormRenderer class >> canvasScaleFactor: newScale [

	canvasScaleFactor := 1 max: newScale asInteger
]

{ #category : 'accessing' }
OSWindowFormRenderer class >> settingsOn: aBuilder [

	<systemsettings>

	(aBuilder group: #canvasScaleFactorGroup)
		parent: #appearance;
		label: 'World renderer canvas scaling';
		with: [
			(aBuilder pickOne: #canvasScaleFactor)
				label: 'Canvas scale factor';
				target: self;
				domainValues: (1 to: 5);
				default: 1.
			(aBuilder setting: #autoSetCanvasScaleFactor)
				label: 'Set canvas scale factor automatically';
				target: self;
				default: true ]
]

{ #category : 'private' }
OSWindowFormRenderer >> canvasScaleFactor [

	^ self class canvasScaleFactor
]

{ #category : 'morphic integration' }
OSWindowFormRenderer >> deferUpdatesWhile: aBlock [

	aBlock value
]

{ #category : 'accessing' }
OSWindowFormRenderer >> drawDuring: aBlock [

	aBlock value: self getCanvas
]

{ #category : 'accessing' }
OSWindowFormRenderer >> form [

	^ form
]

{ #category : 'accessing' }
OSWindowFormRenderer >> form: anObject [

	form := anObject
]

{ #category : 'accessing' }
OSWindowFormRenderer >> getCanvas [

	| scale formCanvas |
	
	scale := self canvasScaleFactor.
	formCanvas := form getCanvas.
	^ scale = 1
		ifTrue: [ formCanvas ]
		ifFalse: [ ScalingCanvas formCanvas: formCanvas scale: scale ]
]

{ #category : 'size' }
OSWindowFormRenderer >> newExtent: newExtent [
	form setExtent: newExtent depth: form depth
]

{ #category : 'accessing' }
OSWindowFormRenderer >> outputExtent [

	self subclassResponsibility
]

{ #category : 'accessing' }
OSWindowFormRenderer >> pixelExtent [
	^ form ifNotNil: [ form extent ] ifNil: [ 1@1 ]
]

{ #category : 'morphic integration' }
OSWindowFormRenderer >> updateAreas: allDamage immediate: forceToScreen [
	"Force all the damage rects to the screen."
	allDamage do: [ :r| self updateRectangle: r ]
]

"
I know how to render a ruler in an Athens canvas.

Note: an instance of me cannot be used to render in more than one canvas.
"
Class {
	#name : #HiRulerRenderer,
	#superclass : #HiRenderer,
	#instVars : [
		'ruler',
		'rulerBuilder'
	],
	#category : #Hiedra-Rendering
}

{ #category : #'instance creation' }
HiRulerRenderer class >> model: aModel athensCanvas: aCanvas ruler: aRuler [
	^ self basicNew
		initializeWithModel: aModel
		athensCanvas: aCanvas
		ruler: aRuler;
		yourself
]

{ #category : #private }
HiRulerRenderer >> canvasPointFor: aNode [
	^ (rulerBuilder centerOfColumnIndex: aNode rulerPoint x) @ (rulerBuilder centerOfRowIndex: aNode rulerPoint y)
]

{ #category : #private }
HiRulerRenderer >> canvasPointsForAll: aCollectionOfNodes [
	^ aCollectionOfNodes collect: [ :rulerPoint | 
		(rulerBuilder centerOfColumnIndex: rulerPoint x) @ (rulerBuilder centerOfRowIndex: rulerPoint y) ]
]

{ #category : #initialize-release }
HiRulerRenderer >> initializeWithModel: aModel athensCanvas: aCanvas ruler: aRuler [
	self initialize.
		
	rulerBuilder := aModel.
	athensCanvas := aCanvas.
	ruler := aRuler.
]

{ #category : #rendering }
HiRulerRenderer >> render [
	self renderLinks.
	self renderNodes.
]

{ #category : #private }
HiRulerRenderer >> renderLinks [
	self flag: #todo. "FIX"

	ruler links do: [ :link |
		link linkBuilder renderer
			rulerBuilder: rulerBuilder;
			athensCanvas: athensCanvas;
			checkpoints: (self canvasPointsForAll: link checkpoints);
			render ]
]

{ #category : #private }
HiRulerRenderer >> renderNodes [
	ruler nodes do: [ :aNode | 
		rulerBuilder nodeBuilder renderer
			athensCanvas: athensCanvas;
			location: (self canvasPointFor: aNode);
			render ]
]

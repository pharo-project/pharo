"
I am like my superclass, can display any scene (object which understands `#renderOn:`  message)
and support free-form pan and zoom. But in addition, I'm expecting that scene object understands following:

`scene handleEvent: event in: sceneView at: position`
 
This method should answer true if scene object wants to handle event by itself, preventing default handling of view (pan & zoom).
The point is coordinates of event in scene coordinate space (not screen coordinate space). The event is MorphicEvent (currently limited only to mouse move&button events).

Note that if scene view enters zooming or panning state, no events will be passed to scene object as long as state is active. 

"
Class {
	#name : #AthensInteractiveScene,
	#superclass : #AthensSceneView,
	#category : #'Athens-Morphic'
}

{ #category : #'event handling' }
AthensInteractiveScene >> eventHandledByScene: evt [

	^  true == (scene handleEvent: evt in: self at: (self eventPositionInScene:evt))
]

{ #category : #'event handling' }
AthensInteractiveScene >> eventPositionInScene: evt [
	| pt |

	pt := evt position - self topLeft.

	^ transform transform: pt
]

{ #category : #'event handling' }
AthensInteractiveScene >> mouseDown: evt [

	((self inState: #panning) or: [ self inState:#zooming ])
		ifFalse: [
			(self eventHandledByScene: evt) ifTrue: [ ^ self ].
		].

	^ super mouseDown: evt
]

{ #category : #'event handling' }
AthensInteractiveScene >> mouseMove: evt [

	((self inState: #panning) or: [ self inState:#zooming ])
		ifFalse: [
			(self eventHandledByScene: evt) ifTrue: [ ^ self ].
		].
	^ super mouseMove: evt
]

{ #category : #'event handling' }
AthensInteractiveScene >> mouseUp: evt [

	((self inState: #panning) or: [ self inState:#zooming ])
		ifFalse: [
			(self eventHandledByScene: evt) ifTrue: [ ^ self ].
		].

	^ super mouseUp:evt
]

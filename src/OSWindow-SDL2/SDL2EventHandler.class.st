"
I am an interface (abstract) used for visiting the raw SDL2 events. 

"
Class {
	#name : #SDL2EventHandler,
	#superclass : #Object,
	#category : 'OSWindow-SDL2'
}

{ #category : #visitor }
SDL2EventHandler >> visitCommonEvent: commonEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitControllerAxisEvent: controllerAxisEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitControllerButtonDownEvent: controllerButtonEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitControllerButtonUpEvent: controllerButtonEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitControllerDeviceAddedEvent: controllerDeviceEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitControllerDeviceRemappedEvent: controllerDeviceEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitControllerDeviceRemovedEvent: controllerDeviceEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitJoyAxisEvent: windowEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitJoyButtonDownEvent: windowEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitJoyButtonUpEvent: windowEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitJoyDeviceAddedEvent: joyDeviceEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitJoyDeviceRemovedEvent: joyDeviceEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitKeyDownEvent: windowEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitKeyUpEvent: windowEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitMouseButtonDownEvent: mouseMotionEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitMouseButtonUpEvent: mouseMotionEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitMouseMotionEvent: mouseMotionEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitMouseWheelEvent: mouseMotionEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitTextInputEvent: windowEvent [
]

{ #category : #visitor }
SDL2EventHandler >> visitWindowEvent: windowEvent [
]

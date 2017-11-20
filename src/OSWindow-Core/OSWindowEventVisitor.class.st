"
i am a abstract visitor for handling events.. 
serving just to cover complete protocol, which user(s) will need to implement in order to act as an OSWindow event handler.
"
Class {
	#name : #OSWindowEventVisitor,
	#superclass : #Object,
	#category : #OSWindow-Core-Events
}

{ #category : #events }
OSWindowEventVisitor >> handleEvent: anEvent [
	anEvent accept: self.
]

{ #category : #visiting }
OSWindowEventVisitor >> visitJoyAxisEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitJoyButtonDownEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitJoyButtonUpEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitJoyDeviceAddedEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitJoyDeviceRemappedEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitJoyDeviceRemovedEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitKeyDownEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitKeyUpEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitMouseButtonPressEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitMouseButtonReleaseEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitMouseMoveEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitMouseWheelEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitTextInputEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitTouchPointerDownEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitTouchPointerMoveEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitTouchPointerUpEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitUnknownEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowCloseEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowDropEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowExposeEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowFocusInEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowFocusOutEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowFrameEvent: anEvent [

]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowMaximizedEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowMinimizedEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowMouseEnterEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowMouseLeaveEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowMoveEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventVisitor >> visitWindowResizeEvent: anEvent [
]

"
I am abstract class, representing an OSWindow handle.
Each instance of OSWindow holds a handle, through which it communicates with underlaying operating system.

The implementation of OSWindowHandle (and subclasses) is highly driver-specific and therefore considered private.
"
Class {
	#name : #OSWindowHandle,
	#superclass : #Object,
	#instVars : [
		'osWindow',
		'renderer'
	],
	#category : #'OSWindow-Core-Utilities'
}

{ #category : #accessing }
OSWindowHandle >> borderless [
	^ false
]

{ #category : #accessing }
OSWindowHandle >> borderless: aBoolean [
	
]

{ #category : #accessing }
OSWindowHandle >> bounds [
	^ self position extent: self extent
]

{ #category : #accessing }
OSWindowHandle >> bounds: newBounds [
	self subclassResponsibility 
]

{ #category : #'mouse capture' }
OSWindowHandle >> captureMouse [
]

{ #category : #accessing }
OSWindowHandle >> clipboardText [
	self subclassResponsibility 
]

{ #category : #accessing }
OSWindowHandle >> clipboardText: aString [
	self subclassResponsibility 
]

{ #category : #accessing }
OSWindowHandle >> extent [
	^ self bounds extent
]

{ #category : #accessing }
OSWindowHandle >> extent: newExtent [
	self subclassResponsibility 
]

{ #category : #accessing }
OSWindowHandle >> fullscreen: aBoolean [
]

{ #category : #accessing }
OSWindowHandle >> hide [
	self subclassResponsibility 
]

{ #category : #accessing }
OSWindowHandle >> icon: aForm [
]

{ #category : #'text input' }
OSWindowHandle >> isTextInputActive [
	^ self subclassResponsibility
]

{ #category : #testing }
OSWindowHandle >> isValid [
	self subclassResponsibility 
]

{ #category : #factory }
OSWindowHandle >> newAthensRenderer [
	self subclassResponsibility
]

{ #category : #factory }
OSWindowHandle >> newFormRenderer: aForm [
	self subclassResponsibility
]

{ #category : #factory }
OSWindowHandle >> newGenericRenderer [
	self subclassResponsibility
]

{ #category : #factory }
OSWindowHandle >> newOpenGLRenderer [
	self subclassResponsibility
]

{ #category : #accessing }
OSWindowHandle >> osWindow [
	
	^ osWindow
]

{ #category : #accessing }
OSWindowHandle >> osWindow: anObject [
	
	osWindow := anObject
]

{ #category : #accessing }
OSWindowHandle >> platformSpecificHandle [
	self subclassResponsibility
]

{ #category : #accessing }
OSWindowHandle >> position [
	^ self subclassResponsibility
]

{ #category : #accessing }
OSWindowHandle >> position: newPosition [
	self subclassResponsibility 
]

{ #category : #accessing }
OSWindowHandle >> prepareExternalResourceForAutoRelease [
	"This hook will allow just create handlers to prepate their external resources for being 
	 releases by the GC (if needed). By default, do nothing."
]

{ #category : #'mouse capture' }
OSWindowHandle >> releaseMouse [
]

{ #category : #accessing }
OSWindowHandle >> renderer [
	
	^ renderer
]

{ #category : #accessing }
OSWindowHandle >> renderer: anObject [
	
	renderer := anObject
]

{ #category : #accessing }
OSWindowHandle >> resizable [
	^ true
]

{ #category : #accessing }
OSWindowHandle >> resizable: aBoolean [
	
]

{ #category : #cursor }
OSWindowHandle >> setMouseCursor: cursorWithMask [
	self setMouseCursor: cursorWithMask mask: cursorWithMask maskForm
]

{ #category : #accessing }
OSWindowHandle >> setMouseCursor: cursor mask: mask [
]

{ #category : #accessing }
OSWindowHandle >> show [
	self subclassResponsibility 
]

{ #category : #'text input' }
OSWindowHandle >> startTextInput [
	self subclassResponsibility
]

{ #category : #'text input' }
OSWindowHandle >> stopTextInput [
	self subclassResponsibility
]

{ #category : #accessing }
OSWindowHandle >> title [
	self subclassResponsibility 
]

{ #category : #accessing }
OSWindowHandle >> title: aTitle [
	self subclassResponsibility 
]

{ #category : #accessing }
OSWindowHandle >> windowId [
	^ nil
]

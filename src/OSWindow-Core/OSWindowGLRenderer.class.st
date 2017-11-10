"
i am abstract OpenGL renderer facade. 
"
Class {
	#name : #OSWindowGLRenderer,
	#superclass : #OSWindowRenderer,
	#category : #OSWindow-Core-OpenGL
}

{ #category : #misc }
OSWindowGLRenderer >> getProcAddress: procName [
	"Gets an OpenGL procedure address."
	self subclassResponsibility
]

{ #category : #misc }
OSWindowGLRenderer >> makeCurrent [
	^ self makeCurrent: window
]

{ #category : #misc }
OSWindowGLRenderer >> makeCurrent: aWindow [
	self subclassResponsibility
]

{ #category : #misc }
OSWindowGLRenderer >> swapBuffers [
	^ self swapBuffers: window
]

{ #category : #misc }
OSWindowGLRenderer >> swapBuffers: aWindow [
	self subclassResponsibility
]

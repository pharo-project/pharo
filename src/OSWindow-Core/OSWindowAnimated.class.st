"
I am an animated window who interacts with the OSWindowRenderThread
"
Class {
	#name : #OSWindowAnimated,
	#superclass : #OSWindow,
	#category : 'OSWindow-Core-Extras'
}

{ #category : #private }
OSWindowAnimated >> destroy [
	OSWindowRenderThread destroyedWindow: self.
	super destroy
]

{ #category : #initialization }
OSWindowAnimated >> initWithAttributes: attributes [
	super initWithAttributes: attributes.
	self registerWindow.
]

{ #category : #private }
OSWindowAnimated >> registerWindow [
	OSWindowRenderThread createdWindow: self.
]

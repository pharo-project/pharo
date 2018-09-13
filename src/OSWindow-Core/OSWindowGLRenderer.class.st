"
i am abstract OpenGL renderer facade. 
"
Class {
	#name : #OSWindowGLRenderer,
	#superclass : #OSWindowRenderer,
	#classVars : [
		'CurrentContext',
		'CurrentWindow'
	],
	#category : #'OSWindow-Core-OpenGL'
}

{ #category : #'context activation' }
OSWindowGLRenderer >> asCurrentDo: aBlock [
	^ self asCurrentDo: aBlock ifFailure: [ ]
]

{ #category : #'context activation' }
OSWindowGLRenderer >> asCurrentDo: aBlock ifFailure: aFailureBlock [
	^ self withWindow: window do: aBlock ifFailure: aFailureBlock
]

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

{ #category : #'context activation' }
OSWindowGLRenderer >> withWindow: aWindow do: aBlock [
	^ self withWindow: aWindow do: aBlock ifFailure: []
]

{ #category : #'context activation' }
OSWindowGLRenderer >> withWindow: aWindow do: aBlock ifFailure: aFailureBlock [
	| oldContext oldWindow |
	OSWindowRenderThread uniqueInstance critical: [ 
		oldContext := CurrentContext.
		oldWindow := CurrentWindow.
		[ 
			(self makeCurrent: aWindow) ifTrue: [
				CurrentContext := self.
				CurrentWindow := aWindow.
				aBlock value
			] ifFalse: [
				aFailureBlock value
			]
		] ensure: [
			(oldContext ~~ CurrentContext or: [ oldWindow ~~ CurrentWindow ]) ifTrue: [ 
				CurrentContext := oldContext.
				CurrentWindow := oldWindow.
				CurrentContext ifNotNil: [ CurrentContext makeCurrent: CurrentWindow ]
			]
		]
	]
]

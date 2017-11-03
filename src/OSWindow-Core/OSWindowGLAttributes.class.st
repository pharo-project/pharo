"
I holding a set of OpenGL attributes, used to initialize an opengl rendering context for a window.
"
Class {
	#name : #OSWindowGLAttributes,
	#superclass : #Object,
	#instVars : [
		'redSize',
		'blueSize',
		'greenSize',
		'alphaSize',
		'depthSize',
		'doubleBuffer',
		'swapControl',
		'stencilSize',
		'minorVersion',
		'majorVersion',
		'profileType'
	],
	#classVars : [
		'DefaultAlphaSize',
		'DefaultBlueSize',
		'DefaultDepthSize',
		'DefaultDoubleBuffer',
		'DefaultGreenSize',
		'DefaultMajorVersion',
		'DefaultMinorVersion',
		'DefaultProfileType',
		'DefaultRedSize',
		'DefaultStencilSize',
		'DefaultSwapControl'
	],
	#category : #OSWindow-Core-OpenGL
}

{ #category : #'as yet unclassified' }
OSWindowGLAttributes class >> defaultAlphaSize [
	^ DefaultAlphaSize
]

{ #category : #'as yet unclassified' }
OSWindowGLAttributes class >> defaultBlueSize [
	^ DefaultBlueSize
]

{ #category : #'as yet unclassified' }
OSWindowGLAttributes class >> defaultDepthSize [
	^ DefaultDepthSize
]

{ #category : #'as yet unclassified' }
OSWindowGLAttributes class >> defaultDoubleBuffer [
	^ DefaultDoubleBuffer
]

{ #category : #'as yet unclassified' }
OSWindowGLAttributes class >> defaultGreenSize [
	^ DefaultGreenSize
]

{ #category : #'as yet unclassified' }
OSWindowGLAttributes class >> defaultRedSize [
	^ DefaultRedSize
]

{ #category : #'as yet unclassified' }
OSWindowGLAttributes class >> defaultStencilSize [
	^ DefaultStencilSize
]

{ #category : #'as yet unclassified' }
OSWindowGLAttributes class >> defaultSwapControl [
	^ DefaultSwapControl
]

{ #category : #'as yet unclassified' }
OSWindowGLAttributes class >> initialize [
	"
	self initialize
	"
	super initialize.
	
	DefaultRedSize := 5.
	DefaultGreenSize := 5.
	DefaultBlueSize := 5.
	DefaultAlphaSize := 5.
	DefaultDepthSize := 16.
	DefaultDoubleBuffer := 1.
	DefaultStencilSize := 0.
	DefaultSwapControl := 0.
	DefaultMajorVersion := 3.
	DefaultMinorVersion := 2.
	DefaultProfileType := 1.
]

{ #category : #accessing }
OSWindowGLAttributes >> alphaSize [
	
	^ alphaSize
]

{ #category : #accessing }
OSWindowGLAttributes >> alphaSize: anObject [
	
	alphaSize := anObject
]

{ #category : #accessing }
OSWindowGLAttributes >> blueSize [
	
	^ blueSize
]

{ #category : #accessing }
OSWindowGLAttributes >> blueSize: anObject [
	
	blueSize := anObject
]

{ #category : #accessing }
OSWindowGLAttributes >> depthSize [
	
	^ depthSize
]

{ #category : #accessing }
OSWindowGLAttributes >> depthSize: anObject [
	
	depthSize := anObject
]

{ #category : #accessing }
OSWindowGLAttributes >> doubleBuffer [
	
	^ doubleBuffer
]

{ #category : #accessing }
OSWindowGLAttributes >> doubleBuffer: anObject [
	
	doubleBuffer := anObject
]

{ #category : #accessing }
OSWindowGLAttributes >> greenSize [
	
	^ greenSize
]

{ #category : #accessing }
OSWindowGLAttributes >> greenSize: anObject [
	
	greenSize := anObject
]

{ #category : #initialization }
OSWindowGLAttributes >> initialize [
	redSize := DefaultRedSize.
	greenSize := DefaultGreenSize.
	blueSize := DefaultBlueSize.
	alphaSize := DefaultAlphaSize.
	depthSize := DefaultDepthSize.
	stencilSize := DefaultStencilSize.
	doubleBuffer := DefaultDoubleBuffer.
	swapControl := DefaultSwapControl.
	minorVersion := DefaultMinorVersion.
	majorVersion := DefaultMajorVersion.
	profileType := DefaultProfileType.
]

{ #category : #accessing }
OSWindowGLAttributes >> majorVersion [
	^ majorVersion
]

{ #category : #accessing }
OSWindowGLAttributes >> majorVersion: anObject [
	majorVersion := anObject
]

{ #category : #accessing }
OSWindowGLAttributes >> minorVersion [
	^ minorVersion
]

{ #category : #accessing }
OSWindowGLAttributes >> minorVersion: anObject [
	minorVersion := anObject
]

{ #category : #accessing }
OSWindowGLAttributes >> profileType [
	^ profileType
]

{ #category : #accessing }
OSWindowGLAttributes >> profileType: anObject [
	profileType := anObject
]

{ #category : #accessing }
OSWindowGLAttributes >> redSize [
	
	^ redSize
]

{ #category : #accessing }
OSWindowGLAttributes >> redSize: anObject [
	
	redSize := anObject
]

{ #category : #accessing }
OSWindowGLAttributes >> stencilSize [
	^ stencilSize
]

{ #category : #accessing }
OSWindowGLAttributes >> stencilSize: aStencilSize [
	stencilSize := aStencilSize
]

{ #category : #accessing }
OSWindowGLAttributes >> swapControl [
	
	^ swapControl
]

{ #category : #accessing }
OSWindowGLAttributes >> swapControl: anObject [
	
	swapControl := anObject
]

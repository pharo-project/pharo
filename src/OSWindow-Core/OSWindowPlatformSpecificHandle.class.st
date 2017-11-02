"
I represent a collection of platform specific handles that can be passed to external graphics rendering APIs such as OpenGL, Vulkan, Direct 3D and Metal.

Instance Variables
	display:		The display handle.
	platform:		The a symbol with the name of the platform.
	surface:		The window drawing surface handle.
	window:		The window handle.

"
Class {
	#name : #OSWindowPlatformSpecificHandle,
	#superclass : #Object,
	#instVars : [
		'platform',
		'display',
		'window',
		'surface'
	],
	#category : 'OSWindow-Core'
}

{ #category : #accessing }
OSWindowPlatformSpecificHandle >> display [
	^ display
]

{ #category : #accessing }
OSWindowPlatformSpecificHandle >> display: anObject [
	display := anObject
]

{ #category : #accessing }
OSWindowPlatformSpecificHandle >> platform [
	^ platform
]

{ #category : #accessing }
OSWindowPlatformSpecificHandle >> platform: anObject [
	platform := anObject
]

{ #category : #accessing }
OSWindowPlatformSpecificHandle >> surface [
	^ surface
]

{ #category : #accessing }
OSWindowPlatformSpecificHandle >> surface: anObject [
	surface := anObject
]

{ #category : #accessing }
OSWindowPlatformSpecificHandle >> window [
	^ window
]

{ #category : #accessing }
OSWindowPlatformSpecificHandle >> window: anObject [
	window := anObject
]

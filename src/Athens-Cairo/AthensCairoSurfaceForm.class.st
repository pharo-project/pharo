"
I'm a form that keeps a surface with the only purpose of prevent GC when is still being used to copy data.

I'm used at AthensCairoSurface>>#asForm
"
Class {
	#name : #AthensCairoSurfaceForm,
	#superclass : #Form,
	#instVars : [
		'surface'
	],
	#category : #'Athens-Cairo-Surface'
}

{ #category : #accessing }
AthensCairoSurfaceForm >> surface [
	^ surface
]

{ #category : #accessing }
AthensCairoSurfaceForm >> surface: anObject [
	surface := anObject
]

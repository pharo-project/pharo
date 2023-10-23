"
I'm a form that keeps a surface with the only purpose of prevent GC when is still being used to copy data.

I'm used at AthensCairoSurface>>#asForm
"
Class {
	#name : 'AthensCairoSurfaceForm',
	#superclass : 'Form',
	#instVars : [
		'surface'
	],
	#category : 'Athens-Cairo-Surface',
	#package : 'Athens-Cairo',
	#tag : 'Surface'
}

{ #category : 'scaling, rotation' }
AthensCairoSurfaceForm >> magnifyBy: scale smoothing: cellSize [

	| form |
	
	form := Form extent: self extent depth: self depth.
	form getCanvas image: self at: 0@0 sourceRect: self boundingBox rule: 34.
	^ form magnifyBy: scale smoothing: cellSize
]

{ #category : 'accessing' }
AthensCairoSurfaceForm >> surface [
	^ surface
]

{ #category : 'accessing' }
AthensCairoSurfaceForm >> surface: anObject [
	surface := anObject
]

"
Athens example using Ballon as surface

 
"
Class {
	#name : #AthensBalloonSurfaceExamples,
	#superclass : #AthensSurfaceExamples,
	#category : #'Athens-Examples-Base'
}

{ #category : #utilities }
AthensBalloonSurfaceExamples class >> newSurface: extent [

	^ AthensBalloonSurface extent: extent
]

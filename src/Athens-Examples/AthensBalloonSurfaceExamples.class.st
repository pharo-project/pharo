"
Athens example using Ballon as surface

 
"
Class {
	#name : #AthensBalloonSurfaceExamples,
	#superclass : #AthensSurfaceExamples,
	#category : #'Athens-Examples-Demos'
}

{ #category : #utilities }
AthensBalloonSurfaceExamples class >> newSurface: extent [
	
	^ AthensBalloonSurface extent: extent
]

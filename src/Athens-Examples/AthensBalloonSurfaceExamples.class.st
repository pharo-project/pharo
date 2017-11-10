"
Athens example using Ballon as surface

 
"
Class {
	#name : #AthensBalloonSurfaceExamples,
	#superclass : #AthensSurfaceExamples,
	#category : #Athens-Examples
}

{ #category : #'instance creation' }
AthensBalloonSurfaceExamples class >> newSurface: extent [
	
	^ AthensBalloonSurface extent: extent
]

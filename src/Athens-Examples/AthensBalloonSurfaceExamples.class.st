"
Athens example using Ballon as surface

 
"
Class {
	#name : 'AthensBalloonSurfaceExamples',
	#superclass : 'AthensSurfaceExamples',
	#category : 'Athens-Examples-Base',
	#package : 'Athens-Examples',
	#tag : 'Base'
}

{ #category : 'utilities' }
AthensBalloonSurfaceExamples class >> newSurface: extent [

	^ AthensBalloonSurface extent: extent
]

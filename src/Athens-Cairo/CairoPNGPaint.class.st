"
I am a surface paint with lazy initialization scheme, that i loading image from .png file
the first time i will be used.

<Do not use me, because i am really Cairo specific>
"
Class {
	#name : #CairoPNGPaint,
	#superclass : #AthensAbstractPaint,
	#instVars : [
		'fileName'
	],
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Paints'
}

{ #category : #'instance creation' }
CairoPNGPaint class >> fileNamed: aFileName [
	| image|
	image := self new.
	image initializeOnFileNamed: aFileName.
	^ image
]

{ #category : #converting }
CairoPNGPaint >> asAthensPaintOn: anAthensCanvas [

	| surface |

	 surface := AthensCairoSurface createFromFile: fileName ifFailed: [ :status |

		status =  CAIRO_STATUS_READ_ERROR ifTrue: [
			self error: 'Cairo couldnt read the file named ', fileName ].
		status = CAIRO_STATUS_FILE_NOT_FOUND ifTrue: [
			self error: 'Cairo cant find the file named ', fileName ].

		^ self error: 'Cannot create surface. Cairo status code: ', status asString
	].

	^ surface asAthensPaintOn: anAthensCanvas
]

{ #category : #initialization }
CairoPNGPaint >> initializeOnFileNamed: aFileName [
	fileName := aFileName
]

"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- BaselineOfSUnit

Load type: 
- linear

Installs:
- Graphics-* packages (Form, Bitmap, GrafPort...)
"
Class {
	#name : #BaselineOfDisplay,
	#superclass : #BaselineOf,
	#instVars : [
		'initializersEnabled'
	],
	#category : #BaselineOfDisplay
}

{ #category : #baseline }
BaselineOfDisplay >> baseline: spec [
	<baseline>

	spec for: #common do: [
		spec preLoadDoIt: #'preload:package:'.
		spec postLoadDoIt: #'postload:package:'.
		
		spec 
			package: 'Graphics-Display Objects';
			package: 'Graphics-Primitives';
			package: 'Graphics-Transformations'.
		spec 
			group: 'default' with: #('Graphics-Display Objects' 'Graphics-Primitives' 'Graphics-Transformations'). ].		

]

{ #category : #actions }
BaselineOfDisplay >> installBitmap [

	"install new Bitmap class to special objects array"

	| array |
	
	array := Smalltalk specialObjectsArray copy.
	array at: 5 put: Bitmap.
	Smalltalk specialObjectsArray becomeForward: array.
]

{ #category : #actions }
BaselineOfDisplay >> postload: loader package: packageSpec [

	| display |
	
	self installBitmap.

	display := (Smalltalk classNamed: #DisplayScreen) new.
	display setExtent: (Smalltalk classNamed: #DisplayScreen) actualScreenSize depth: 2.
	Smalltalk globals at: #Display put: display.
	display beDisplay.

	Cursor classPool at: #CurrentCursor put: Cursor new.
	Cursor webLink. "set the webLink cursor shape"
					
	Cursor initialize.
	DisplayScreen initialize.
	BitBlt initialize.
	Form initialize.
	
	MCMethodDefinition initializersEnabled: initializersEnabled.
	
	
]

{ #category : #actions }
BaselineOfDisplay >> preload: loader package: packageSpec [

	initializersEnabled := MCMethodDefinition initializersEnabled.
	MCMethodDefinition initializersEnabled: false.
	

	

]

"
I am a test library using the SameThread runner
"
Class {
	#name : #TFTestLibraryUsingSameThreadRunner,
	#superclass : #FFILibrary,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #'accessing - platform' }
TFTestLibraryUsingSameThreadRunner >> calloutAPIClass [

	^ TFCalloutAPI 
]

{ #category : #'accessing - platform' }
TFTestLibraryUsingSameThreadRunner >> macLibraryName [ 

	^ 'libTestLibrary.dylib'
]

{ #category : #accessing }
TFTestLibraryUsingSameThreadRunner >> runner [ 
	
	^ TFSameThreadRunner uniqueInstance
]

{ #category : #'accessing - platform' }
TFTestLibraryUsingSameThreadRunner >> unixModuleName [

	^ 'libTestLibrary.so'
]

{ #category : #'accessing - platform' }
TFTestLibraryUsingSameThreadRunner >> win32LibraryName [
 
	^ 'TestLibrary.dll'
]

Class {
	#name : 'TFUFFITestClassWithTrait',
	#superclass : 'FFILibrary',
	#traits : 'TFFITraitForTest',
	#classTraits : 'TFFITraitForTest classTrait',
	#category : 'ThreadedFFI-UFFI-Tests',
	#package : 'ThreadedFFI-UFFI-Tests'
}

{ #category : 'accessing - platform' }
TFUFFITestClassWithTrait >> macLibraryName [

	^ 'libTestLibrary.dylib'
]

{ #category : 'accessing' }
TFUFFITestClassWithTrait >> shortCallout [

	^ self ffiCall: #(int shortCallout())
]

{ #category : 'accessing - platform' }
TFUFFITestClassWithTrait >> unixLibraryName [

	^ 'libTestLibrary.so'
]

{ #category : 'accessing - platform' }
TFUFFITestClassWithTrait >> win32LibraryName [

	^ 'TestLibrary.dll'
]

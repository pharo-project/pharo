"
I'm an util to answer module libraries that can have different names in different platforms. 
I just implement a double dispatch with platform.
"
Class {
	#name : #FFILibrary,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'UnifiedFFI-Libraries'
}

{ #category : #converting }
FFILibrary class >> asFFILibrary [
	^ self uniqueInstance
]

{ #category : #'instance creation' }
FFILibrary class >> new [
	self error: 'Use #uniqueInstance'
]

{ #category : #'instance creation' }
FFILibrary class >> uniqueInstance [ 
	self = FFILibrary 
		ifTrue: [ self error: 'I''m an abstract class, use one of my children.' ].
	^ uniqueInstance ifNil: [ uniqueInstance := super new ]
]

{ #category : #converting }
FFILibrary >> asFFILibrary [ 
	^ self
]

{ #category : #'library path' }
FFILibrary >> ffiLibraryName [
	^ self moduleName
]

{ #category : #'accessing platform' }
FFILibrary >> macModuleName [ 
	^ self subclassResponsibility
]

{ #category : #accessing }
FFILibrary >> moduleName [ 
	^ Smalltalk platform ffiModuleName: self
]

{ #category : #'accessing platform' }
FFILibrary >> unix32ModuleName [
	"Point to #unixModuleName to keep backward compatibility." 
	^ self unixModuleName
]

{ #category : #'accessing platform' }
FFILibrary >> unix64ModuleName [
	"Point to #unixModuleName to keep backward compatibility." 
	^ self unixModuleName
]

{ #category : #'accessing platform' }
FFILibrary >> unixModuleName [
	"Kept for backward compatibility. 
	 Users should use unix32* or unix64*"
	^ self subclassResponsibility
]

{ #category : #'accessing platform' }
FFILibrary >> win32ModuleName [ 
	^ self subclassResponsibility
]

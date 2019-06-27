"
A visitor to be subclassed to write platform specific code without needing to add extension methods on OSPlatform subclasses.
"
Class {
	#name : #OSPlatformVisitor,
	#superclass : #Object,
	#category : #'System-Platforms-Base'
}

{ #category : #visiting }
OSPlatformVisitor >> visit [ 

	^ OSPlatform current accept: self
]

{ #category : #visiting }
OSPlatformVisitor >> visitGeneric: aPlatform [
	
]

{ #category : #visiting }
OSPlatformVisitor >> visitMacOS: aPlatform [

	^ self visitGeneric: aPlatform
]

{ #category : #visiting }
OSPlatformVisitor >> visitMacOSX: aPlatform [

	^ self visitMacOS: aPlatform
]

{ #category : #visiting }
OSPlatformVisitor >> visitUnix32: aPlatform [

	^ self visitUnix: aPlatform
]

{ #category : #visiting }
OSPlatformVisitor >> visitUnix64: aPlatform [

	^ self visitUnix: aPlatform
]

{ #category : #visiting }
OSPlatformVisitor >> visitUnix: aPlatform [

	^ self visitGeneric: aPlatform
]

{ #category : #visiting }
OSPlatformVisitor >> visitWindows32: aPlatform [ 

	^ self visitWindows: aPlatform
]

{ #category : #visiting }
OSPlatformVisitor >> visitWindows64: aPlatform [ 

	^ self visitWindows: aPlatform
]

{ #category : #visiting }
OSPlatformVisitor >> visitWindows: aPlatform [ 

	^ self visitGeneric: aPlatform
]

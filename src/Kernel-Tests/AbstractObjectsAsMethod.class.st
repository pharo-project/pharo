"
This class shows the API that an object needs to implement if it is supposed to be used as a method.

The vm calls run:with:in:, see the subclass for a simple example.
"
Class {
	#name : 'AbstractObjectsAsMethod',
	#superclass : 'Object',
	#category : 'Kernel-Tests-Methods',
	#package : 'Kernel-Tests',
	#tag : 'Methods'
}

{ #category : 'compatibility' }
AbstractObjectsAsMethod >> cachePragmas [
]

{ #category : 'compatibility' }
AbstractObjectsAsMethod >> flushCache [
]

{ #category : 'compatibility' }
AbstractObjectsAsMethod >> methodClass: aMethodClass [
]

{ #category : 'compatibility' }
AbstractObjectsAsMethod >> origin [
	^self class
]

{ #category : 'compatibility' }
AbstractObjectsAsMethod >> package [
	^self class package
]

{ #category : 'compatibility' }
AbstractObjectsAsMethod >> pragmas [
	^#()
]

{ #category : 'compatibility' }
AbstractObjectsAsMethod >> pragmasDo: aBlock [
	"We do not manage pragmas."

	
]

{ #category : 'compatibility' }
AbstractObjectsAsMethod >> removeFromPackage [
]

{ #category : 'compatibility' }
AbstractObjectsAsMethod >> selector: aSymbol [
]

{ #category : 'asserting' }
AbstractObjectsAsMethod >> shouldBePackaged [

	^ false
]

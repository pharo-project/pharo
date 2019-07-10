"
I am a root of hierarchy of first class tags which can mark objects.

For example ClyAbstractItemTag is created to mark abstract classes and methods.

ClyBrowserItem provides suitable methods for tagging:
	- item markWith: aSimpleTagClass
	- item isMarkedWith: aSimpleTagClass
	
I provide singleton instance for my subclasses:
	ClyAbstractItemTag instance 
So tagging items do not produce garbage
"
Class {
	#name : #ClySimpleTag,
	#superclass : #ClyProperty,
	#classInstVars : [
		'instance'
	],
	#category : #'Calypso-NavigationModel-Tag'
}

{ #category : #singleton }
ClySimpleTag class >> instance [
	^instance ifNil: [ instance := self new ] 
]

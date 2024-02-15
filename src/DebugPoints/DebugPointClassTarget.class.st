"
I am a debug point target, that installs metalinks in the scope of a class.
"
Class {
	#name : 'DebugPointClassTarget',
	#superclass : 'DebugPointTarget',
	#category : 'DebugPoints-Base',
	#package : 'DebugPoints',
	#tag : 'Base'
}

{ #category : 'installing' }
DebugPointClassTarget >> install: aLink [

	self install: aLink on: self targetClass
]

{ #category : 'private ' }
DebugPointClassTarget >> install: aLink on: aClassOrObject [

	self subclassResponsibility
]

{ #category : 'description' }
DebugPointClassTarget >> scope [

	^ 'class {1}' format: { self targetClass }
]

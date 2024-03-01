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

{ #category : 'accessing - classes' }
DebugPointClassTarget class >> hitAnnouncementClass [

	^ self subclassResponsibility
]

{ #category : 'announcements' }
DebugPointClassTarget >> hitAnnouncementOn: aDebugPoint inContext: aContext [

	^ self class hitAnnouncementClass
		  on: aDebugPoint
		  nodes:
		  { (aContext compiledCode sourceNodeForPC: aContext pc) parent }
			  asSet
]

{ #category : 'installing' }
DebugPointClassTarget >> install: aLink [

	self install: aLink on: self targetClass
]

{ #category : 'private' }
DebugPointClassTarget >> install: aLink on: aClassOrObject [

	self subclassResponsibility
]

{ #category : 'description' }
DebugPointClassTarget >> scope [

	^ 'class {1}' format: { self targetClass }
]

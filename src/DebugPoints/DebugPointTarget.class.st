"
I am the base class that serevs as wrappers for debug point targets.

I define how to install a metalink on me, and how to update the DebugPoint browser when it is selected in the debug point browser.
"
Class {
	#name : 'DebugPointTarget',
	#superclass : 'Object',
	#instVars : [
		'#target => WeakSlot'
	],
	#category : 'DebugPoints-Base',
	#package : 'DebugPoints',
	#tag : 'Base'
}

{ #category : 'scope creation' }
DebugPointTarget >> beForObject: anObject [

	^ DebugPointObjectTarget new
		  targetInstance: anObject;
		  subTarget: self;
		  yourself
]

{ #category : 'actions' }
DebugPointTarget >> browse [

	self subclassResponsibility 
]

{ #category : 'initialization' }
DebugPointTarget >> initialize [

	self class initializeSlots: self.
	super initialize.
]

{ #category : 'installing' }
DebugPointTarget >> install: aLink [

	self subclassResponsibility
]

{ #category : 'description' }
DebugPointTarget >> scope [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
DebugPointTarget >> target [

	^ target
]

{ #category : 'accessing' }
DebugPointTarget >> target: anObject [

	target := anObject
]

{ #category : 'accessing' }
DebugPointTarget >> targetClass [

	self subclassResponsibility
]

{ #category : 'description' }
DebugPointTarget >> targetString [

	^ self target printString
]

"
I am a debug point target that installs a metalink on an instance variable slot of a class.


"
Class {
	#name : 'DebugPointInstanceVariableTarget',
	#superclass : 'DebugPointClassTarget',
	#instVars : [
		'accessStrategy'
	],
	#category : 'DebugPoints-Implementations',
	#package : 'DebugPoints',
	#tag : 'Implementations'
}

{ #category : 'accessing - classes' }
DebugPointInstanceVariableTarget class >> hitAnnouncementClass [

	^ VariableDebugPointHit
]

{ #category : 'accessing' }
DebugPointInstanceVariableTarget >> accessStrategy [

	^ accessStrategy
]

{ #category : 'accessing' }
DebugPointInstanceVariableTarget >> accessStrategy: aSymbol [

	accessStrategy := aSymbol
]

{ #category : 'actions' }
DebugPointInstanceVariableTarget >> browse [

	((Smalltalk tools toolNamed: #browser) openOnClass:
			 self instanceVariable owningClass)
		switchToVariables;
		selectVariableNamed: self instanceVariable name
]

{ #category : 'announcements' }
DebugPointInstanceVariableTarget >> hitAnnouncementOn: aDebugPoint inContext: aContext [

	| announcement |
	announcement := super
		                hitAnnouncementOn: aDebugPoint
		                inContext: aContext.
	^ announcement
		  variableValue:
			  (aContext readVariableNamed: self instanceVariable name);
		  yourself
]

{ #category : 'accessing' }
DebugPointInstanceVariableTarget >> impactedNodes [

	^ self instanceVariable accessingNodesFor: self accessStrategy
]

{ #category : 'private' }
DebugPointInstanceVariableTarget >> install: aLink on: aClassOrObject [

	aClassOrObject
		link: aLink
		toSlotNamed: self instanceVariable name
		option: self accessStrategy
]

{ #category : 'accessing' }
DebugPointInstanceVariableTarget >> instanceVariable [

	^ self target
]

{ #category : 'accessing' }
DebugPointInstanceVariableTarget >> instanceVariable: anInstanceVariable [

	self target: anInstanceVariable
]

{ #category : 'testing' }
DebugPointInstanceVariableTarget >> isReadOnly [

	^ accessStrategy = #read
]

{ #category : 'testing' }
DebugPointInstanceVariableTarget >> isReadWrite [

	^ accessStrategy = #all
]

{ #category : 'testing' }
DebugPointInstanceVariableTarget >> isWriteOnly [

	^ accessStrategy = #write
]

{ #category : 'removing' }
DebugPointInstanceVariableTarget >> removeFromMethod: aMethod for: aDebugPoint [

	(aDebugPoint nodes select: [ :n |
		 n methodNode compiledMethod == aMethod ]) do: [ :n |
		aDebugPoint removeNode: n ]
]

{ #category : 'accessing' }
DebugPointInstanceVariableTarget >> targetClass [

	^ self instanceVariable owningClass
]

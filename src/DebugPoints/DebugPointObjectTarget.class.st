"
I am a debug point target that decorates a debug point class target to adapt it to the scope of a specific instance scope
"
Class {
	#name : 'DebugPointObjectTarget',
	#superclass : 'DebugPointTarget',
	#instVars : [
		'subTarget'
	],
	#category : 'DebugPoints-Base',
	#package : 'DebugPoints',
	#tag : 'Base'
}

{ #category : 'api - model' }
DebugPointObjectTarget >> beForObject: anObject [

	^ self
		  targetInstance: anObject;
		  yourself
]

{ #category : 'description' }
DebugPointObjectTarget >> browse [

	^ self subTarget browse
]

{ #category : 'announcements' }
DebugPointObjectTarget >> hitAnnouncementOn: aDebugPoint inContext: aContext [

	^ self subTarget hitAnnouncementOn: aDebugPoint inContext: aContext
]

{ #category : 'actions' }
DebugPointObjectTarget >> install: aLink [

	self subTarget install: aLink on: self targetInstance
]

{ #category : 'removing' }
DebugPointObjectTarget >> removeFromMethod: aMethod for: aDebugPoint [

	self subTarget removeFromMethod: aMethod for: aDebugPoint
]

{ #category : 'scope' }
DebugPointObjectTarget >> resetObjectScope [

	^ self subTarget
]

{ #category : 'description' }
DebugPointObjectTarget >> scope [

	^ self targetInstance
]

{ #category : 'description' }
DebugPointObjectTarget >> scopeString [

	^ 'obj {1}' format: { self scope }
]

{ #category : 'accessing' }
DebugPointObjectTarget >> subTarget [

	^ subTarget
]

{ #category : 'accessing' }
DebugPointObjectTarget >> subTarget: anObject [

	subTarget := anObject
]

{ #category : 'accessing' }
DebugPointObjectTarget >> targetClass [

	^ self subTarget targetClass
]

{ #category : 'accessing' }
DebugPointObjectTarget >> targetInstance [

	^ self target

	
]

{ #category : 'accessing' }
DebugPointObjectTarget >> targetInstance: anObject [

	self target: anObject
]

{ #category : 'description' }
DebugPointObjectTarget >> targetString [

	^ self subTarget targetString
]

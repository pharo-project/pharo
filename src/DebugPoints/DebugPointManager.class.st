"
I am responsible for managing the debug points.

An exception is the removal of Debug points, which is handled in the ""remove"" method of DebugPoint instances.
"
Class {
	#name : 'DebugPointManager',
	#superclass : 'Object',
	#category : 'DebugPoints-Base',
	#package : 'DebugPoints',
	#tag : 'Base'
}

{ #category : 'announcement handling' }
DebugPointManager class >> handleClassRemoved: anAnnouncement [

	DebugPoint all copy do: [ :debugPoint |
		debugPoint link methods
			detect: [ :m | m methodClass = anAnnouncement classRemoved ]
			ifFound: [ debugPoint remove ] ]
]

{ #category : 'announcement handling' }
DebugPointManager class >> handleMethodModified: anAnnouncement [
	self removeFromMethod: anAnnouncement oldMethod
]

{ #category : 'announcement handling' }
DebugPointManager class >> handleMethodRemoved: anAnnouncement [
	self removeFromMethod: anAnnouncement method
]

{ #category : 'class initialization' }
DebugPointManager class >> initialize [
	self registerInterestToSystemAnnouncement
]

{ #category : 'private - helpers' }
DebugPointManager class >> installDebugPoint: dp withBehaviors: aListOfBehaviorClasses [

	| ml |
	ml := dp metaLink.

	dp link: ml.
	dp install.
	DebugPoint add: dp.

	"adding behaviors"
	aListOfBehaviorClasses do: [ :bhc | dp addBehavior: bhc new ].

	self notifyDebugPointAdded: dp.

	^ dp
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass On: aNode [

	^self installNew: aDebugPointClass On: aNode withBehaviors: {  }.
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass On: aNode forObject: anObject [

	^self installNew: aDebugPointClass On: aNode forObject: anObject withBehaviors: {  }.
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass On: aNode forObject: anObject withBehaviors: aListOfBehaviorClasses [
	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	| dp |
	dp := aDebugPointClass new
		      node: aNode;
		      targetInstance: anObject;
		      yourself.

	^ self installDebugPoint: dp withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass On: aNode withBehaviors: aListOfBehaviorClasses [
	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	| dp |
	dp := aDebugPointClass new
		      node: aNode;
		      yourself.

	^ self
		  installDebugPoint: dp
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableAccessNamed: aSlotNameSymbol [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #all
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableAccessNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #all
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableNamed: aSlotNameSymbol accessStrategy: anAccessStrategySymbol [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: anAccessStrategySymbol
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableNamed: aSlotNameSymbol accessStrategy: anAccessStrategySymbol withBehaviors: aListOfBehaviorClasses [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	| dp instanceVariable |
	instanceVariable := anObject class slotNamed: aSlotNameSymbol.
	dp := aDebugPointClass new
		      instanceVariable: instanceVariable
		      accessStrategy: anAccessStrategySymbol;
		      targetInstance: anObject;
		      yourself.

	^ self installDebugPoint: dp withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableReadNamed: aSlotNameSymbol [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #read
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableReadNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #read
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableWriteNamed: aSlotNameSymbol [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #write
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableWriteNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #write
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableAccessNamed: aSlotNameSymbol [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #all
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableAccessNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #all
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableNamed: aSlotNameSymbol accessStrategy: anAccessStrategySymbol [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: anAccessStrategySymbol
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableNamed: aSlotNameSymbol accessStrategy: anAccessStrategySymbol withBehaviors: aListOfBehaviorClasses [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	| dp instanceVariable |
	instanceVariable := aClass slotNamed: aSlotNameSymbol.
	dp := aDebugPointClass new
		      instanceVariable: instanceVariable
		      accessStrategy: anAccessStrategySymbol;
		      yourself.

	^ self installDebugPoint: dp withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableReadNamed: aSlotNameSymbol [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #read
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableReadNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #read
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableWriteNamed: aSlotNameSymbol [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #write
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableWriteNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"access strategy can be #all or #read or #write"

	"this should be the single point of installation for debug points, removal is in the remove method of debug point instances"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #write
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'announcements' }
DebugPointManager class >> notifyDebugPointAdded: aDebugPoint [

	| announcement |
	announcement := DebugPointAdded 
		on: aDebugPoint
		nodes: aDebugPoint link nodes.
	SystemAnnouncer uniqueInstance announce: announcement
]

{ #category : 'announcements' }
DebugPointManager class >> notifyDebugPointChanged: aDebugPoint [
	
	| announcement |
	announcement := DebugPointChanged
		on: aDebugPoint
		nodes: aDebugPoint link nodes.
	SystemAnnouncer uniqueInstance announce: announcement
]

{ #category : 'announcements' }
DebugPointManager class >> notifyDebugPointRemoved: aDebugPoint fromNodes: nodes [
	"nodes have to be seperate because they will be removed from the debugpoint before this method is called"
	| announcement |
	announcement := DebugPointRemoved
		on: aDebugPoint 
		nodes: nodes.
	SystemAnnouncer uniqueInstance announce: announcement.

		

]

{ #category : 'announcements - registration' }
DebugPointManager class >> registerInterestToSystemAnnouncement [
	<systemEventRegistration>

	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak when: MethodRemoved send: #handleMethodRemoved: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodModified send: #handleMethodModified: to: self.
	SystemAnnouncer uniqueInstance weak when: ClassRemoved send: #handleClassRemoved: to: self
]

{ #category : 'removing' }
DebugPointManager class >> removeFromMethod: aMethod [

	DebugPoint all copy do: [ :debugPoint | 
		debugPoint link methods
			detect: [ :m | m == aMethod ]
			ifFound: [ debugPoint removeFromMethod: aMethod ] ]
]

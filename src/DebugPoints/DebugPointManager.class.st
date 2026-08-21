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

	DebugPoint all copy do: [ :dp |
		dp removeFromClass: anAnnouncement classRemoved ]
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

	dp install.
	DebugPoint add: dp.

	"Adding behaviors"
	aListOfBehaviorClasses do: [ :bhc | dp addBehavior: bhc new ].

	self notifyDebugPointAdded: dp.

	^ dp
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableAccessNamed: aSlotNameSymbol [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #all
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableAccessNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #all
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableNamed: aSlotNameSymbol accessStrategy: anAccessStrategySymbol [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: anAccessStrategySymbol
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableNamed: aSlotNameSymbol accessStrategy: anAccessStrategySymbol withBehaviors: aListOfBehaviorClasses [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

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
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #read
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableReadNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #read
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableWriteNamed: aSlotNameSymbol [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #write
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass forObject: anObject onVariableWriteNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  forObject: anObject
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #write
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableAccessNamed: aSlotNameSymbol [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #all
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableAccessNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #all
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableNamed: aSlotNameSymbol accessStrategy: anAccessStrategySymbol [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"
	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: anAccessStrategySymbol
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableNamed: aSlotNameSymbol accessStrategy: anAccessStrategySymbol withBehaviors: aListOfBehaviorClasses [
		
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	| dp instanceVariable |
	instanceVariable := aClass lookupVar: aSlotNameSymbol.
	dp := aDebugPointClass new
		      instanceVariable: instanceVariable
		      accessStrategy: anAccessStrategySymbol;
		      yourself.

	^ self installDebugPoint: dp withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableReadNamed: aSlotNameSymbol [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #read
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableReadNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #read
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableWriteNamed: aSlotNameSymbol [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	"Access strategy can be #all or #read or #write"

	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #write
		  withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass inClass: aClass onVariableWriteNamed: aSlotNameSymbol withBehaviors: aListOfBehaviorClasses [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."
	
	"Access strategy can be #all or #read or #write"
	^ self
		  installNew: aDebugPointClass
		  inClass: aClass
		  onVariableNamed: aSlotNameSymbol
		  accessStrategy: #write
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass on: aNode [

	^ self installNew: aDebugPointClass on: aNode withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass on: aNode forObject: anObject [

	^self installNew: aDebugPointClass on: aNode forObject: anObject withBehaviors: {  }
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass on: aNode forObject: anObject withBehaviors: aListOfBehaviorClasses [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	| dp |
	dp := aDebugPointClass new
		      node: aNode;
		      targetInstance: anObject;
		      yourself.

	^ self installDebugPoint: dp withBehaviors: aListOfBehaviorClasses
]

{ #category : 'API' }
DebugPointManager class >> installNew: aDebugPointClass on: aNode withBehaviors: aListOfBehaviorClasses [
	"This is the single point of installation for debug points, removal is in the remove method of debug point instances."

	| dp |
	dp := aDebugPointClass new
		      node: aNode;
		      yourself.

	^ self
		  installDebugPoint: dp
		  withBehaviors: aListOfBehaviorClasses
]

{ #category : 'announcements' }
DebugPointManager class >> notifyDebugPointAdded: aDebugPoint [

	| announcement |
	announcement := DebugPointAdded on: aDebugPoint nodes: aDebugPoint link nodes.
	self codeSupportAnnouncer announce: announcement
]

{ #category : 'announcements' }
DebugPointManager class >> notifyDebugPointChanged: aDebugPoint [

	| announcement |
	announcement := DebugPointChanged on: aDebugPoint nodes: aDebugPoint link nodes.
	self codeSupportAnnouncer announce: announcement
]

{ #category : 'announcements' }
DebugPointManager class >> notifyDebugPointHit: aDebugPoint inContext: aContext [

	| announcement |
	announcement := aDebugPoint hitAnnouncementOn: aDebugPoint inContext: aContext.
	self codeSupportAnnouncer announce: announcement
]

{ #category : 'announcements' }
DebugPointManager class >> notifyDebugPointRemoved: aDebugPoint fromNodes: nodes [
	"Nodes have to be separate because they will be removed from the debugpoint before this method is called."

	| announcement |
	announcement := DebugPointRemoved on: aDebugPoint nodes: nodes.
	self codeSupportAnnouncer announce: announcement
]

{ #category : 'announcements - registration' }
DebugPointManager class >> registerInterestToSystemAnnouncement [

	self codeChangeAnnouncer unsubscribe: self.
	self codeChangeAnnouncer weak
		when: MethodRemoved send: #handleMethodRemoved: to: self;
		when: MethodModified send: #handleMethodModified: to: self;
		when: ClassRemoved send: #handleClassRemoved: to: self
]

{ #category : 'removing' }
DebugPointManager class >> removeFromMethod: aMethod [

	DebugPoint all copy do: [ :debugPoint | 
		debugPoint link methods
			detect: [ :m | m == aMethod ]
			ifFound: [ debugPoint removeFromMethod: aMethod ] ]
]

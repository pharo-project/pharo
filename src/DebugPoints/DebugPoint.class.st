"
I am the base class for all debug point classes. New Debug points should be implemented as subclasses.
"
Class {
	#name : 'DebugPoint',
	#superclass : 'Object',
	#instVars : [
		'name',
		'enabled',
		'behaviors',
		'arguments',
		'metaLink',
		'properties',
		'target'
	],
	#classVars : [
		'AllDebugPoints'
	],
	#category : 'DebugPoints-Base',
	#package : 'DebugPoints',
	#tag : 'Base'
}

{ #category : 'adding' }
DebugPoint class >> add: aDebugPoint [
	self all add: aDebugPoint.
]

{ #category : 'accessing' }
DebugPoint class >> all [
	^AllDebugPoints ifNil: [ AllDebugPoints := Set new. ]
]

{ #category : 'removing' }
DebugPoint class >> remove: aDebugPoint [

	self all remove: aDebugPoint ifAbsent: [].
	
]

{ #category : 'API' }
DebugPoint >> addBehavior: aDebugPointBehavior [
	"adding a new behavior"
	(self getBehavior: aDebugPointBehavior class) ifNotNil: [ :bh | ^bh ].
	self behaviors add: aDebugPointBehavior.
	aDebugPointBehavior debugPoint: self.
	DebugPointManager notifyDebugPointChanged: self.
	
]

{ #category : 'accessing' }
DebugPoint >> arguments [

	^ arguments ifNil: [ arguments := Dictionary new ]
]

{ #category : 'accessing' }
DebugPoint >> arguments: aDictionary [
	arguments := aDictionary
]

{ #category : 'accessing' }
DebugPoint >> behaviors [
	^behaviors
]

{ #category : 'accessing' }
DebugPoint >> behaviors: aSetOfBehaviors [
	behaviors := aSetOfBehaviors 
]

{ #category : 'actions' }
DebugPoint >> browse [

	^ self target browse
]

{ #category : 'reflective operations' }
DebugPoint >> doesNotUnderstand: message [
	"forward not understood messages to behaviors, 'delegation complements composition' "
	self behaviors do: [ :bh | 
		(bh respondsTo: message selector)
			ifTrue: [ ^bh perform: message selector withEnoughArguments: message arguments ]
		].
	
	^super doesNotUnderstand: message
]

{ #category : 'accessing' }
DebugPoint >> enabled [
	^enabled
]

{ #category : 'accessing' }
DebugPoint >> enabled: aBoolean [
	enabled := aBoolean.
	DebugPointManager notifyDebugPointChanged: self.
]

{ #category : 'accessing' }
DebugPoint >> getArgument: aSymbol [
	^self arguments at: aSymbol
]

{ #category : 'accessing' }
DebugPoint >> getBehavior: aBehaviorClass [
	^self behaviors detect: [ :bh | bh class = aBehaviorClass ] ifNone: [nil]
]

{ #category : 'API' }
DebugPoint >> hitWithContext: aContext [
	"returns false if the execution should be stopped, else true"

	<debuggerCompleteToSender>
	self enabled ifFalse: [ ^ false ].
	self saveContext: aContext.
	^ self behaviors allSatisfy: [ :behavior | behavior execute ]
]

{ #category : 'initialization' }
DebugPoint >> initialize [ 
	enabled := true.
	self behaviors: (SortedCollection sortUsing: [ :elem1 :elem2 | elem1 priority >= elem2 priority]).
]

{ #category : 'installing' }
DebugPoint >> install [

	self link ifNil: [ ^ self ].
	self link uninstall.
	self target install: self link
]

{ #category : 'API' }
DebugPoint >> instanceVariable: aSlot accessStrategy: aSymbol [

	target := DebugPointInstanceVariableTarget new
		          instanceVariable: aSlot;
		          accessStrategy: aSymbol;
		          yourself.

	name := 'var_{1}_{2}' format: {
			        aSlot name.
			        aSymbol }
]

{ #category : 'accessing' }
DebugPoint >> link [
	^metaLink
]

{ #category : 'accessing' }
DebugPoint >> link: aMetaLink [
	metaLink := aMetaLink 
]

{ #category : 'default values' }
DebugPoint >> metaLink [
	^(MetaLink new
		metaObject: self;
		options: #(+ optionCompileOnLinkInstallation);
		selector: #hitWithContext:;
		arguments: #(context) ).
]

{ #category : 'accessing' }
DebugPoint >> name [
	^name ifNil: [ #Debugpoint ]
]

{ #category : 'accessing' }
DebugPoint >> name: aSymbol [
	name := aSymbol
]

{ #category : 'API' }
DebugPoint >> node: aNode [

	target := DebugPointNodeTarget new
		          node: aNode;
		          yourself
]

{ #category : 'accessing' }
DebugPoint >> properties [ 
	^properties ifNil: [ properties := Dictionary new. ]
]

{ #category : 'removing' }
DebugPoint >> remove [
	| announcement nodes |
	nodes := self link nodes copy.
	self behaviors do: [ :bh | bh remove ].
	self class remove: self.
	self link ifNotNil: [self link uninstall].
	
	announcement := DebugPointRemoved
	on: self
	nodes: nodes.
	SystemAnnouncer uniqueInstance announce: announcement.
	
	
	
]

{ #category : 'API' }
DebugPoint >> removeBehavior: aDebugPointBehaviorClass [
	"removing a behavior"
	|behavior|
	behavior := (self getBehavior: aDebugPointBehaviorClass).
	behavior remove.
	self behaviors remove: behavior.
	DebugPointManager notifyDebugPointChanged: self.

]

{ #category : 'removing' }
DebugPoint >> removeFromMethod: aMethod [
	"only used when editing methods with debug points"

	| nodes |
	nodes := self link nodes copy.
	self behaviors do: [ :bh | bh remove ].
	self class remove: self.

	DebugPointManager notifyDebugPointRemoved: self fromNodes: nodes
]

{ #category : 'scope' }
DebugPoint >> resetObjectScope [

	target resetObjectScope
]

{ #category : 'API' }
DebugPoint >> saveContext: aContext [

	self arguments at: #context put: aContext
]

{ #category : 'description' }
DebugPoint >> scope [

	^ self target scope
]

{ #category : 'accessing' }
DebugPoint >> target [

	^ target
]

{ #category : 'description' }
DebugPoint >> targetClass [

	^ self target targetClass
]

{ #category : 'scope' }
DebugPoint >> targetInstance: anObject [

	target := self target beForObject: anObject.
	self install
]

{ #category : 'description' }
DebugPoint >> targetString [

	^ self target targetString
]

{ #category : 'accessing' }
DebugPoint >> type [

	^ #Debugpoint
]

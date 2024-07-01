"
I am the base class for all debug point classes. New Debug points should be implemented as subclasses.
"
Class {
	#name : 'DebugPoint',
	#superclass : 'Object',
	#instVars : [
		'name',
		'enabled',
		'checkBehaviors',
		'sideEffectBehaviors',
		'arguments',
		'properties',
		'target',
		'link'
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

{ #category : 'world menu' }
DebugPoint class >> menuCommandOn: aBuilder [

	<worldMenu>
	(aBuilder item: #'Remove all Debug Points')
		parent: #Debug;
		order: -2;
		help: 'Remove all the debug points of the image.';
		action: [ self removeAll ].

	(aBuilder item: #'Enable all Debug Points')
		parent: #Debug;
		order: -1;
		help: 'Enable all the debug points of the image.';
		action: [ self all do: #enable ].

	(aBuilder item: #'Disable all Debug Points')
		parent: #Debug;
		order: 0;
		help: 'Disable all the debug points of the image.';
		action: [ self all do: #disable ].
	aBuilder withSeparatorAfter
]

{ #category : 'removing' }
DebugPoint class >> remove: aDebugPoint [

	self all remove: aDebugPoint ifAbsent: [].
	
]

{ #category : 'removing' }
DebugPoint class >> removeAll [

	<script>
	self all copy do: #remove
]

{ #category : 'API' }
DebugPoint >> addBehavior: aDebugPointBehavior [
	"adding a new behavior"

	(self getBehavior: aDebugPointBehavior class) ifNotNil: [ :bh | ^ bh ].
	self behaviors add: aDebugPointBehavior.
	aDebugPointBehavior addToDebugPoint: self.
	aDebugPointBehavior debugPoint: self.
	DebugPointManager notifyDebugPointChanged: self
]

{ #category : 'adding' }
DebugPoint >> addCheckBehavior: aConditionBehavior [

	checkBehaviors add: aConditionBehavior
]

{ #category : 'adding' }
DebugPoint >> addSideEffectBehavior: aSideEffectBehavior [

	sideEffectBehaviors add: aSideEffectBehavior
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

	^ checkBehaviors , sideEffectBehaviors
]

{ #category : 'actions' }
DebugPoint >> browse [

	^ self target browse
]

{ #category : 'accessing' }
DebugPoint >> checkBehaviors [

	^ checkBehaviors
]

{ #category : 'protocol' }
DebugPoint >> disable [

	self enabled: false
]

{ #category : 'reflective operations' }
DebugPoint >> doesNotUnderstand: message [
	"forward not understood messages to behaviors, 'delegation complements composition' "

	self behaviors do: [ :bh |
		(bh respondsTo: message selector) ifTrue: [
			^ bh
				  perform: message selector
				  withEnoughArguments: message arguments ] ].

	^ super doesNotUnderstand: message
]

{ #category : 'protocol' }
DebugPoint >> enable [

	self enabled: true
]

{ #category : 'accessing' }
DebugPoint >> enabled [

	^ enabled
]

{ #category : 'accessing' }
DebugPoint >> enabled: aBoolean [

	enabled := aBoolean.
	DebugPointManager notifyDebugPointChanged: self
]

{ #category : 'accessing' }
DebugPoint >> getArgument: aSymbol [

	^ self arguments at: aSymbol
]

{ #category : 'accessing' }
DebugPoint >> getBehavior: aBehaviorClass [

	^ self behaviors
		  detect: [ :bh | bh class = aBehaviorClass ]
		  ifNone: [ nil ]
]

{ #category : 'announcements' }
DebugPoint >> hitAnnouncementOn: aDebugPoint inContext: aContext [

	^ self target hitAnnouncementOn: aDebugPoint inContext: aContext
]

{ #category : 'API' }
DebugPoint >> hitWithContext: aContext [
	"executes side effect behaviors if and only if this debug point is enabled and if all check behaviors return true"

	<debuggerCompleteToSender>
	DebugPointManager notifyDebugPointHit: self inContext: aContext.
	self enabled ifFalse: [ ^ false ].
	self saveContext: aContext.
	(self checkBehaviors allSatisfy: [ :behavior | behavior execute ])
		ifFalse: [ ^ false ].
	self sideEffectBehaviors do: [ :behavior | behavior execute ].
	^ true
]

{ #category : 'initialization' }
DebugPoint >> initialize [

	enabled := true.
	checkBehaviors := SortedCollection sortUsing: [ :elem1 :elem2 |
		                  elem1 priority >= elem2 priority ].
	sideEffectBehaviors := SortedCollection sortUsing: [ :elem1 :elem2 |
		                       elem1 priority >= elem2 priority ]
]

{ #category : 'installing' }
DebugPoint >> install [

	self link ifNil: [ ^self ].
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

	^ link ifNil: [ link := self metaLink ]
]

{ #category : 'accessing' }
DebugPoint >> link: aMetaLink [

	link := aMetaLink
]

{ #category : 'default values' }
DebugPoint >> metaLink [

	^ MetaLink new
		  metaObject: self;
		  options: #( #+ optionCompileOnLinkInstallation );
		  selector: #hitWithContext:;
		  arguments: #( context )
]

{ #category : 'accessing' }
DebugPoint >> name [

	^ name ifNil: [ #Debugpoint ]
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
DebugPoint >> nodes [

	^ self link nodes
]

{ #category : 'accessing' }
DebugPoint >> properties [

	^ properties ifNil: [ properties := Dictionary new ]
]

{ #category : 'removing' }
DebugPoint >> remove [

	| nodes |
	nodes := self link nodes copy.
	self behaviors do: [ :bh | bh remove ].
	self class remove: self.
	self link ifNotNil: [ self link uninstall ].

	DebugPointManager notifyDebugPointRemoved: self fromNodes: nodes
]

{ #category : 'API' }
DebugPoint >> removeBehavior: aDebugPointBehaviorClass [
	"removing a behavior"

	| behavior |
	behavior := self getBehavior: aDebugPointBehaviorClass.
	behavior remove.
	behavior removeFromDebugPoint: self.
	DebugPointManager notifyDebugPointChanged: self
]

{ #category : 'removing' }
DebugPoint >> removeCheckBehavior: aCheckBehavior [

	checkBehaviors remove: aCheckBehavior
]

{ #category : 'removing' }
DebugPoint >> removeFromClass: aClass [
	"If the removed class is the class we target or one of its superclasses,
	then after that class is removed we have no reason to exist. We need to uninstall."

	(self targetClass = aClass or: [
		 self targetClass allSuperclasses includes: aClass ]) ifTrue: [
		self remove.
		^ self ].

	"We removed a aClass, but it is not our target class nor one of its superclasses.
	It is then one of its subclasses, we can continue to exist since the variable
	we target still exist in the target class.
	However, we need to remove the nodes of the removed class from our metalink"
	(self link nodes select: [ :n | n methodNode methodClass == aClass ])
		do: [ :n | self link removeNode: n ]
]

{ #category : 'removing' }
DebugPoint >> removeFromMethod: aMethod [
	"only used when editing methods with debug points"

	self target removeFromMethod: aMethod for: self
]

{ #category : 'removing' }
DebugPoint >> removeNode: aNode [

	self link removeNode: aNode
]

{ #category : 'removing' }
DebugPoint >> removeSideEffectBehavior: aSideEffectBehavior [

	sideEffectBehaviors remove: aSideEffectBehavior
]

{ #category : 'scope' }
DebugPoint >> resetObjectScope [

	target := target resetObjectScope.
	self install
]

{ #category : 'API' }
DebugPoint >> saveContext: aContext [

	self arguments at: #context put: aContext
]

{ #category : 'scope' }
DebugPoint >> scope [

	^ self target scope
]

{ #category : 'scope' }
DebugPoint >> scopeString [

	^ self target scopeString
]

{ #category : 'accessing' }
DebugPoint >> sideEffectBehaviors [

	^ sideEffectBehaviors
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

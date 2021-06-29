"
A Breakpoint is an object used for stopping the execution of the program in a determined instruction, normally with debugging purposes. 
There are three types of breakpoints:
-halt once: These are triggered once, and then removed.
-halt always: Once installed, they have to be explicitly removed (by using #remove on the instance side, or #removeAll on class side)
-halt on condition: Before being installed prompts the user for a condition (a block with a boolean expression), and it is only triggered when that condition is met.

Breakpoints are installed via SmartSuggestions menu (available for RBMethodNode, RBMessageNode and RBAssignmentNode), or programmatically:

""(Breakpoint inMethod: (Bar>>#foo2) inNode: ((Bar>>#foo2) ast allChildren at: 9) )
break: #when: withArguments: { [ RFCounter counter isZero ] }
""


"
Class {
	#name : #Breakpoint,
	#superclass : #Object,
	#instVars : [
		'#condition',
		'#link',
		'#node',
		'#level',
		'#options',
		'#targetInstance => WeakSlot',
		'#oneShot',
		'#enabled'
	],
	#classVars : [
		'#AllBreakpoints'
	],
	#category : #'Reflectivity-Breakpoints'
}

{ #category : #all }
Breakpoint class >> addBreakpoint: aBreakpoint [
	self all add: aBreakpoint.
	self notifyBreakpointAdded: aBreakpoint
]

{ #category : #all }
Breakpoint class >> all [ 
	^ AllBreakpoints ifNil: [ AllBreakpoints := OrderedCollection new ]
]

{ #category : #'API - object-centric' }
Breakpoint class >> breakOn: aSelector inObject: anObject [
	|ast|
	ast := (anObject class lookupSelector: aSelector) ast.
	^self breakOnAST: ast inObject: anObject
]

{ #category : #'API - object-centric' }
Breakpoint class >> breakOnAST: aNode inObject: anObject [
	|bp|	
	bp := self new.
	bp node: aNode.
	bp scopeTo: anObject.
	bp install.
	^bp
]

{ #category : #all }
Breakpoint class >> browseAll [
	<script>
	| methods |
	methods := self all flatCollect: [ :breakpoint | breakpoint link methods ].
	Smalltalk tools messageList browse: methods
]

{ #category : #checkCondition }
Breakpoint class >> checkBreakConditionValue: aValue [
	(aValue isKindOf: Boolean) ifTrue: [ ^ aValue ].
	Error signal: 'Breakpoint condition returned a non boolean value'.
]

{ #category : #cleanup }
Breakpoint class >> cleanUp [
	self removeAll
]

{ #category : #all }
Breakpoint class >> debugWorldMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Remove all Breakpoints')
		parent: #Breakpoints;
		order: 2;
		help: 'Remove all the breakpoints of the image.';
		action: [ Breakpoint removeAll ]
]

{ #category : #'system announcements' }
Breakpoint class >> handleClassRemoved: anAnnouncement [

	self all copy do: [ :breakpoint | 
		breakpoint link methods
			detect: [ :m | m methodClass = anAnnouncement classRemoved ]
			ifFound: [ breakpoint removeFromClass: anAnnouncement classRemoved ] ]
]

{ #category : #'system announcements' }
Breakpoint class >> handleMethodModified: anAnnouncement [
	self removeFromMethod: anAnnouncement oldMethod
]

{ #category : #'system announcements' }
Breakpoint class >> handleMethodRemoved: anAnnouncement [
	self removeFromMethod: anAnnouncement method
]

{ #category : #initialization }
Breakpoint class >> initialize [
	self registerInterestToSystemAnnouncement 
]

{ #category : #querying }
Breakpoint class >> isInstalledIn: aMethod [

	self all do: [ :breakpoint |
		breakpoint link methods 
			detect: [ :m | m == aMethod ]
			ifFound: [ ^ true ]
	].

	^ false
]

{ #category : #'observers - experimental' }
Breakpoint class >> notifyBreakpointAdded: aBreakpoint [
	| announcement |
	announcement := BreakpointAdded 
		on: aBreakpoint
		nodes: aBreakpoint link nodes.
	SystemAnnouncer uniqueInstance announce: announcement
	
]

{ #category : #'observers - experimental' }
Breakpoint class >> notifyBreakpointHit: aBreakpoint inContext: aContext node: node [
	| announcement |
	announcement := BreakpointHit
		on: aBreakpoint
		nodes: {node}.
	SystemAnnouncer uniqueInstance announce: announcement
]

{ #category : #'observers - experimental' }
Breakpoint class >> notifyBreakpointRemoved: aBreakpoint fromNodes: nodes [
	| announcement |
	announcement := BreakpointRemoved
		on: aBreakpoint
		nodes: nodes.
	SystemAnnouncer uniqueInstance announce: announcement
]

{ #category : #'class initialization' }
Breakpoint class >> registerInterestToSystemAnnouncement [
	<systemEventRegistration>

	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak when: MethodRemoved send: #handleMethodRemoved: to: self.
	SystemAnnouncer uniqueInstance weak when: MethodModified send: #handleMethodModified: to: self.
	SystemAnnouncer uniqueInstance weak when: ClassRemoved send: #handleClassRemoved: to: self
]

{ #category : #'observers - experimental' }
Breakpoint class >> registerObserver: anObject [
	"Do not use this method, it is there for compatibility with old users.
	Instead, register your object yourself to 'SystemAnnouncer uniqueInstance' like below, but only for the announcements you need"
	SystemAnnouncer uniqueInstance when: BreakpointAdded send: #update: to: anObject.
	SystemAnnouncer uniqueInstance when: BreakpointHit send: #update: to: anObject.
	SystemAnnouncer uniqueInstance when: BreakpointRemoved send: #update: to: anObject.
	
]

{ #category : #cleanup }
Breakpoint class >> removeAll [ 
	<script>
	self all copy do: #remove
]

{ #category : #all }
Breakpoint class >> removeBreakpoint: aBreakpoint [
	| nodes |
	nodes := aBreakpoint link nodes copy.
	self all remove: aBreakpoint.
	self notifyBreakpointRemoved: aBreakpoint fromNodes: nodes
]

{ #category : #cleanup }
Breakpoint class >> removeFrom: aNode [
	aNode breakpoints do: [:breakpoint| breakpoint remove]
]

{ #category : #cleanup }
Breakpoint class >> removeFromMethod: aMethod [

	self all copy do: [ :breakpoint | 
		breakpoint link methods
			detect: [ :m | m == aMethod ]
			ifFound: [ breakpoint removeFromMethod: aMethod ] ]
]

{ #category : #'observers - experimental' }
Breakpoint class >> unregisterObserver: anObject [
	"Do not use this method, it is there for compatibility with old users.
	Instead, unsubscribe yourself from 'SystemAnnouncer uniqueInstance' directly (like shown below)"
	SystemAnnouncer uniqueInstance unsubscribe: anObject
]

{ #category : #api }
Breakpoint >> always [
	self link: self breakLink


]

{ #category : #api }
Breakpoint >> breakInContext: aContext node: aNode [
	 <debuggerCompleteToSender>
	 self class notifyBreakpointHit: self inContext: aContext node: aNode.
	 self isEnabled ifFalse: [ ^ self ].
	 self oneShot ifTrue: [ self disable ].
	 Break break
]

{ #category : #links }
Breakpoint >> breakLink [
	"for now it should just halt in base level"

	^ MetaLink new
		  metaObject: self;
		  selector: #breakInContext:node:;
		  options: options;
		  arguments: #(#context #node)
]

{ #category : #links }
Breakpoint >> breakLinkConditional [	
	^self breakLink condition: condition arguments: #(context)
]

{ #category : #api }
Breakpoint >> condition: aCondition [
	condition := aCondition.
	self link: self breakLinkConditional
]

{ #category : #api }
Breakpoint >> disable [
	enabled := false
]

{ #category : #api }
Breakpoint >> enable [
	enabled := true
]

{ #category : #initialization }
Breakpoint >> initialize [
	
	self class initializeSlots: self.
	options := #(+ optionCompileOnLinkInstallation)
]

{ #category : #install }
Breakpoint >> install [
	self setAsNodeProperty.
	self isObjectCentric
		ifTrue: [ self targetInstance link: self link toAST: self node.
			self class addBreakpoint: self.
			^ self ].
	self node link: self link.
	self class addBreakpoint: self 
]

{ #category : #testing }
Breakpoint >> isEnabled [
	^enabled ifNil:[enabled := true]
]

{ #category : #testing }
Breakpoint >> isObjectCentric [
	^self targetInstance notNil
]

{ #category : #testing }
Breakpoint >> isVariableBreakpoint [
	^false
]

{ #category : #accessing }
Breakpoint >> level [
	^level
]

{ #category : #accessing }
Breakpoint >> level: aLevel [
	level := aLevel
]

{ #category : #accessing }
Breakpoint >> link [
	^link ifNil: [ link := self breakLink ]
]

{ #category : #accessing }
Breakpoint >> link: aMetaLink [
	link := aMetaLink
]

{ #category : #accessing }
Breakpoint >> node [ 
	 ^ node
]

{ #category : #accessing }
Breakpoint >> node: aNode [
	node := aNode
]

{ #category : #api }
Breakpoint >> once [
	oneShot := true
]

{ #category : #accessing }
Breakpoint >> oneShot [
	^oneShot ifNil:[oneShot := false]
]

{ #category : #accessing }
Breakpoint >> options [
	^options
]

{ #category : #accessing }
Breakpoint >> options: anArray [
	options := anArray
]

{ #category : #removing }
Breakpoint >> remove [
	self removeFromNodeProperty.
	self class removeBreakpoint: self.
	link uninstall
]

{ #category : #removing }
Breakpoint >> removeFromClass: aClass [
	self class removeBreakpoint: self
]

{ #category : #removing }
Breakpoint >> removeFromMethod: aMethod [
	self remove
]

{ #category : #removing }
Breakpoint >> removeFromNodeProperty [
	self node removeBreakpoint: self
	
]

{ #category : #api }
Breakpoint >> scopeTo: anInstance [
	self targetInstance: anInstance
]

{ #category : #install }
Breakpoint >> setAsNodeProperty [
	self node addBreakpoint: self
	
]

{ #category : #accessing }
Breakpoint >> targetInstance [
	^targetInstance
]

{ #category : #accessing }
Breakpoint >> targetInstance: anObject [
	targetInstance := anObject
]

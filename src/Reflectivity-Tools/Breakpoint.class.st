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
		'condition',
		'link',
		'node',
		'level',
		'options'
	],
	#classVars : [
		'AllBreakpoints'
	],
	#category : #'Reflectivity-Tools-Breakpoints'
}

{ #category : #all }
Breakpoint class >> browseAll [
	<script>
	| methods |
	methods := AllBreakpoints flatCollect: [ :breakpoint | breakpoint link methods ].
	Smalltalk tools messageList browse: methods
]

{ #category : #checkCondition }
Breakpoint class >> checkBreakConditionValue: aValue [
	(aValue isKindOf: Boolean) ifTrue: [ ^ aValue ].
	Error signal: 'Breakpoint condition returned a non boolean value'.
]

{ #category : #cleanup }
Breakpoint class >> cleanUp [
	self removeAll.
]

{ #category : #'system announcements' }
Breakpoint class >> handleClassRemoved: anAnnouncement [
	AllBreakpoints copy do: [ :breakpoint |
		breakpoint link methods
			detect: [ :m | m methodClass = anAnnouncement classRemoved ]
			ifFound: [ AllBreakpoints remove: breakpoint ] ]
]

{ #category : #'system announcements' }
Breakpoint class >> handleMethodModified: anAnnouncement [
	self removeBreakpointsFromMethod: anAnnouncement oldMethod
]

{ #category : #'system announcements' }
Breakpoint class >> handleMethodRemoved: anAnnouncement [
	self removeBreakpointsFromMethod: anAnnouncement method
]

{ #category : #initialization }
Breakpoint class >> initialize [
	AllBreakpoints := OrderedCollection new.
	self registerInsterestToSystemAnnouncement 
]

{ #category : #querying }
Breakpoint class >> isInstalledIn: aMethod [

	AllBreakpoints do: [ :breakpoint |
		breakpoint link methods 
			detect: [ :m | m == aMethod ]
			ifFound: [ ^ true ]
	].

	^ false
]

{ #category : #'class initialization' }
Breakpoint class >> registerInsterestToSystemAnnouncement [
	<systemEventRegistration>
	SystemAnnouncer uniqueInstance
		unsubscribe: self.
	SystemAnnouncer uniqueInstance weak subscribe: MethodRemoved send: #handleMethodRemoved: to: self.
	SystemAnnouncer uniqueInstance weak subscribe: MethodModified send: #handleMethodModified: to: self.
	SystemAnnouncer uniqueInstance weak subscribe: ClassRemoved send: #handleClassRemoved: to: self.
]

{ #category : #cleanup }
Breakpoint class >> removeAll [ 
	<script>
	AllBreakpoints copy do: #remove.
]

{ #category : #removing }
Breakpoint class >> removeBreakpointsFromMethod: aMethod [ 
	AllBreakpoints copy do: [ :breakpoint |
		breakpoint link methods
			detect: [ :m | m == aMethod ]
			ifFound: [ AllBreakpoints remove: breakpoint ] ]
]

{ #category : #cleanup }
Breakpoint class >> removeFrom: aNode [
	| links breakpointsToRemove |
	links := aNode beforeLinks select: [ :link | link metaObject = Break].
	breakpointsToRemove := AllBreakpoints select: [ :br | links includes: br link].
	breakpointsToRemove do: #remove.
]

{ #category : #api }
Breakpoint >> always [
	self link: self breakLink.	


]

{ #category : #links }
Breakpoint >> breakLink [
 	"for now it should just halt in base level"
	^ MetaLink new
		metaObject: Break;
		selector: #break;
		options: options
]

{ #category : #links }
Breakpoint >> breakLinkConditional [	
	^self breakLink condition: condition arguments: #(context)
]

{ #category : #links }
Breakpoint >> breakLinkOneShot [
	^ self breakLink optionOneShot: true. 
]

{ #category : #api }
Breakpoint >> condition: aCondition [
	condition := aCondition.
	self link: self breakLinkConditional.
]

{ #category : #initialization }
Breakpoint >> initialize [
	options := #(+ optionCompileOnLinkInstallation)
]

{ #category : #install }
Breakpoint >> install [
	AllBreakpoints add: self.
	self node link: self link
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
	^link ifNil: [ link := self breakLink]
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
	self link: self breakLinkOneShot.	
	
]

{ #category : #accessing }
Breakpoint >> options: anArray [
	options := anArray
]

{ #category : #install }
Breakpoint >> remove [
	AllBreakpoints remove: self.
	link uninstall
]

"
I am serving as a system-wide hub for various tools.
Instead of using direct class names for tools, which is not always nice because can introduce inter-package
dependencies, i provide a uniform way for accessing tools, via Smalltalk global:

Smalltalk tools someToolName,

where 'someToolName' is a name of the tool , under which some tool are registered.

Tools are adding themselves to registry by implementing #registerToolsOn: message at class side, for example:

MyClass>>registerToolsOn: registry
   registry register: self as: #myTool

Registers a MyClass as a tool under name #myTool, and can be accessed via:

Smalltalk tools myTool 

and avoid putting 'MyClass' reference into code.

You can add  ' self registerToolsOn: Smalltalk tools ' in class #initialize,
so your new tool(s) can be registered during package loading.

To reset tool registry to defaults , use: 
Smalltalk resetTools.

Also registry storing a map of inspector types. 
This is to replace an old scheme which using #inspectorClass , and introducing dependencies.
Now, an inspecting class dont needs to have any knowledge about its specialized inspector.
Instead, a specialized inspector could tell registry that it is available for inspecting instances of given class (see #registerInspector:for:  senders).
This allows to avoid dependency from instance class to its inspector class or using extension methods.

WARNING: The point of the tools registry is NOT to be a facade full of protocols but to be a registration for tools that get loaded. So, the tools registry should NOT hold specific tool's protocol.  the tool's client should invoke the correct methods of the tool.

----------- 
The category 'menu' is for backward compatibility with ToolSet. We should remove it.


"
Class {
	#name : #ToolRegistry,
	#superclass : #Object,
	#instVars : [
		'tools',
		'announcer'
	],
	#category : #'Tool-Registry'
}

{ #category : #announcer }
ToolRegistry >> announcer [
	announcer ifNil: [ announcer := Announcer new ].
	
	^ announcer
]

{ #category : #misc }
ToolRegistry >> browseDirectToolReferences [
	"Smalltalk tools browseDirectToolReferences"
	"Browse all direct references of all of my registered tools.
	Useful for converting calls like:

		MyTool foo
	into:
		Smalltalk tools myTool foo 	"
	
	| bindings |
	
	bindings := tools values select: [:each | each value isBehavior ]
		thenCollect: [:each | each value binding ].
	bindings := bindings copyWith: (Smalltalk bindingOf: #ToolSet).

	^  SystemNavigation new
		browseMessageList: (
			SystemNavigation new allMethodsSelect: [:m |
				(m literals select: [:lit| lit isVariableBinding ]) includesAnyOf: bindings ] )
		name: 'All direct references to tools to fix'
]

{ #category : #'handling DNU' }
ToolRegistry >> doesNotUnderstand: aMessage [
	"Return a tool identified by a message selector.
	We need to handle special case, when no debugger registered.
	"
	| tool |
	tool := tools at: aMessage selector ifAbsent: nil.
	tool ifNotNil: [ ^ tool value ].

	^ (aMessage selector == #debugger) 
		ifTrue: [ 
			Smalltalk 
				logError: 'No debugger installed to handle debug request.' 
				inContext: thisContext.
		
			nil. "Smalltalk snapshot: false andQuit: true."
		]
		ifFalse: [ super doesNotUnderstand: aMessage ]

]

{ #category : #testing }
ToolRegistry >> hasToolNamed: aToolName [
	^ tools includesKey: aToolName 
]

{ #category : #misc }
ToolRegistry >> informNoDebugger [
	(self includes: #uiManager) 
		ifTrue: [
			self uiManager inform: 'Cannot open debugger' ]
		ifFalse: [
			"If we throw an exception here, this can lead to recursion.
			Any idea what else we can do here?"
			Processor activeProcess terminate ]
]

{ #category : #initialization }
ToolRegistry >> initDefaultToolSet [
	
	"Query the system for registering tools"
	"we cannot afford to show warnings because not all tools e.g Debugger, Inspector are registered yet"
	
	[	
	Object withAllSubclassesDo: [:ea | 
	"evaluate only for classes which directly implement this message,
	not via subclassing"
		(ea isMeta  and: [
			ea includesSelector: #registerToolsOn: ]) ifTrue: [
			 ea instanceSide registerToolsOn: self  ]].
	] on: Warning do: [:ex | ex resume ]
]

{ #category : #initialization }
ToolRegistry >> initialize [

	"symbols used as keys "
	tools := IdentityDictionary new.  
	
	self initDefaultToolSet
]

{ #category : #tools }
ToolRegistry >> inspector [
	"understand by Object, so we need to work around not getting a DNU"
	^self doesNotUnderstand: (Message selector: #inspector)
]

{ #category : #menu }
ToolRegistry >> menuItems [
	"Answer the menu items available for this tool set"

	^#(
		('System Browser' 			#openClassBrowser)
		-
		('Playground'				#openWorkspace)
		('Transcript' 				#openTranscript)
		('File Browser'				#openFileList)
		-
		('Test Runner'				#openTestRunner)
		('Process Browser' 			#openProcessBrowser)
		-
		('Monticello Browser'		#openMonticelloBrowser)
		('Change Sorter'			#openChangeSorter)
	)

]

{ #category : #menu }
ToolRegistry >> openClassBrowser [
	^ self browser open
]

{ #category : #menu }
ToolRegistry >> openFileList [
	self fileList open
]

{ #category : #menu }
ToolRegistry >> openMonticelloBrowser [
	self monticelloBrowser open
	
]

{ #category : #menu }
ToolRegistry >> openProcessBrowser [
	self using: #processBrowser do: [:tool |
		tool open
		]

]

{ #category : #menu }
ToolRegistry >> openRecentChangesLog [
	self changeList browseRecentLog
]

{ #category : #menu }
ToolRegistry >> openTestRunner [
	self using: #testRunner do: [:tool | tool open ]
]

{ #category : #menu }
ToolRegistry >> openTranscript [
	^ self transcript open
]

{ #category : #menu }
ToolRegistry >> openWorkspace [
	^ self workspace open
]

{ #category : #registration }
ToolRegistry >> register: anObject as: aNameSelector [
	"Register a tool class under given name"
	tools at: aNameSelector put: anObject.
	self announcer announce: (ToolRegistryToolRegistered tool: anObject name: aNameSelector) 
]

{ #category : #registration }
ToolRegistry >> remove: aName [

	| toolToRemove |
	
	"remove tool with given name"
	toolToRemove := tools removeKey: aName ifAbsent: [].
	self announcer announce: (ToolRegistryToolRegistered tool: toolToRemove name: aName) 
]

{ #category : #announcer }
ToolRegistry >> resetAnnouncer [

	"unregister all current weak subscriptions because it can cause a memory leak"
	announcer subscriptions subscriptions
 		select: [:each | each isKindOf: WeakAnnouncementSubscription] 
		thenDo: [:each | each weakRegistry remove: each subscriber ifAbsent: []]. 
	Smalltalk garbageCollect.
		
	announcer := nil.
]

{ #category : #tools }
ToolRegistry >> transcript [
	"No need to register transcript. But for uniformity,
	implement this method, so transcript can be accessed via
	Smalltalk tools transcript , as any other tool "
	^ Transcript
]

{ #category : #misc }
ToolRegistry >> using: aToolName do: aBlock [
	"Evaluate a block for tool found under corresponding name or silently ignore if its not found or nil"
	| tool |
	
	tool := tools at: aToolName ifAbsent: [ ^ self ].
	tool ifNotNil: [ ^ aBlock value: tool value ]
]

{ #category : #announcer }
ToolRegistry >> whenToolRegistered: aBlock [

	self announcer weak when: ToolRegistryToolRegistered do: aBlock 
]

{ #category : #announcer }
ToolRegistry >> whenToolUnregistered: aBlock [

	self announcer weak when: ToolRegistryToolUnregistered do: aBlock 
]

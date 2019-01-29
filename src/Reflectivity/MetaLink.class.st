"
Metalinks are used to annotate other AST nodes. An annotated AST is expanded, compiled and executed on the fly thanks to the ReflectiveMethod/CompiledMethod Twin. 

For a given node metalinks can be put at different positions:

- before: The metalink is executed before the execution of the node.  
- instead: The metalink is executed insted the node.
- after: The metalink is executed after the execution of the node.
(... later: onError,  Do we have an #around instead of #instead?)

Not all the nodes provide all the position. For example, literals don't provide onError and onSuccess positions.

metaObject: The target object to call
selector: send this selector
arguments

condition:  turn link on/off 
level: Meta Level at which the link is active
------ Examples -----

MetaLink new 
	metaObject: Halt;
	selector: #now.
	
MetaObject new 
	metaObject: [ self halt ];
	selector: #value.
"
Class {
	#name : #MetaLink,
	#superclass : #Object,
	#instVars : [
		'arguments',
		'condition',
		'conditionArguments',
		'control',
		'level',
		'metaObject',
		'nodes',
		'selector',
		'options'
	],
	#classInstVars : [
		'linkInstaller'
	],
	#category : #'Reflectivity-Core'
}

{ #category : #options }
MetaLink class >> defaultOptions [
	^ #(
	+ optionInlineMetaObject            "meta object is inlined by default."
	+ optionInlineCondition              "condition is inlined by default."
	- optionCompileOnLinkInstallation   "generate compiledMethod on link installation"
	- optionOneShot                     "remove link after first activation"
	- optionMetalevel                   "force level: 0 for the link"
	- optionDisabledLink                "links are active by default"
	+ optionWeakAfter 						  "do not use #ensure: for #after "
	- optionAnnounce                    "do we announce adding / removing links? Slow"
	- argsAsArray                       "pass all arguments in one array. By default off as it adds an array creation"
	)
]

{ #category : #accessing }
MetaLink class >> linkInstaller [
	^linkInstaller 
		ifNil: [ linkInstaller := MetaLinkInstaller new ]
]

{ #category : #cleanup }
MetaLink class >> uninstallAll [
	<script>
	self linkInstaller uninstallSuperJumpLinks.
	self allInstances do: #uninstall.
	self linkInstaller unsubscribeFromAnnouncers.
	linkInstaller := nil
]

{ #category : #accessing }
MetaLink >> allReifications [
	| reifications |
	"we only check reification, that is symbols."
	reifications := (self arguments select: [:each | each isSymbol]) asSet. 
	self metaObject isSymbol ifTrue: [reifications add: self metaObject].
	
	self conditionArguments ifNil: [  
		self condition ifNotNil: [reifications addAll: (self reificationsFor: self condition)]]
		ifNotNil: [reifications addAll: self conditionArguments ].
	
	^reifications
]

{ #category : #installing }
MetaLink >> announceChange [
	self optionAnnounce ifTrue: [SystemAnnouncer uniqueInstance announce: (MetalinkChanged new link: self)]
]

{ #category : #accessing }
MetaLink >> arguments [
	^arguments
]

{ #category : #'link api' }
MetaLink >> arguments: args [
	arguments := args.
	self invalidate.
]

{ #category : #installing }
MetaLink >> checkForCompatibilityWith: aNode [
	| supported |
	
	supported := Set new.
	RFReification subclasses do: [:plugin |
		(plugin entities anySatisfy: [:class | aNode isKindOf: class]) ifTrue: [
		supported add: plugin key]].
	self allReifications do: [ :each | (supported includes: each) ifFalse: [^false  ]  ].
	^true
]

{ #category : #accessing }
MetaLink >> condition [
	^condition
]

{ #category : #'link api' }
MetaLink >> condition: aCondition [
	condition:= aCondition.
	self optionInlineCondition ifTrue: [^self invalidate].
	nodes do: [ :node |  | methodNode |
		methodNode := node methodNode.
		(methodNode metaLinkOptionsFromClassAndMethod includes: #optionInlineCondition)
			 ifTrue: [ methodNode method invalidate ] ].
	

	

]

{ #category : #'link api' }
MetaLink >> condition: aCondition arguments: anArray [
	self condition: aCondition.
	conditionArguments := anArray.

	

]

{ #category : #accessing }
MetaLink >> conditionArguments [
	^conditionArguments
]

{ #category : #accessing }
MetaLink >> control [
	^control
	
]

{ #category : #'link api' }
MetaLink >> control: aRFLinkControl [
	control:= aRFLinkControl.
	self invalidate.
]

{ #category : #printing }
MetaLink >> definitionString [
	^String streamContents: [ :str |
		str nextPutAll: 'MetaLink new'.
		str cr;tab.
		str nextPutAll: 'metaObject: '; nextPutAll: metaObject printString; nextPut: $;.
		str cr;tab.
		str nextPutAll: 'selector: '; nextPutAll: selector printString; nextPut: $;.
		str cr;tab.
		str nextPutAll: 'arguments: '; nextPutAll: arguments printString; nextPut: $;.
		str cr;tab.
		str nextPutAll: 'control: '; nextPutAll: control printString; nextPut: $;.
		self hasMetaLevel ifTrue: [  
			str cr;tab.
			str nextPutAll: 'level: '; nextPutAll: level printString; nextPut: $;.
			].
		self hasCondition ifTrue: [
			str cr;tab.
			str nextPutAll: 'condition: '; nextPutAll: condition printString; nextPut: $;.
			].
		str cr;tab.
		str nextPutAll: 'options: '; nextPutAll: options asArray printString; nextPut: $..
	]
]

{ #category : #'reflective api' }
MetaLink >> disable [
	"turn off link reflectively while keeping the condition"
	self hasReifiedCondition
		ifFalse: [self condition: (RFCondition for: condition)].
	self condition disable.
]

{ #category : #private }
MetaLink >> doesNotUnderstand: message [

	(message selector isUnary and: [ message selector beginsWith: 'option'] )
		ifTrue: [ ^ options includes: message selector ].
		
	^ super doesNotUnderstand: message
]

{ #category : #'reflective api' }
MetaLink >> enable [
	"turn on link reflectively if it has been turned off"
	self hasReifiedCondition
		ifTrue: [self condition enable].
	

]

{ #category : #testing }
MetaLink >> hasCondition [	
	^condition notNil
]

{ #category : #testing }
MetaLink >> hasMetaLevel [
	^level notNil
]

{ #category : #testing }
MetaLink >> hasNodes [
	^ nodes notEmpty
]

{ #category : #options }
MetaLink >> hasOption: aSymbol [
	^options includes: aSymbol
]

{ #category : #testing }
MetaLink >> hasReifiedCondition [
	self hasCondition ifFalse: [ ^false ].
	^self condition isKindOf: RFCondition
]

{ #category : #initialization }
MetaLink >> initialize [
	nodes := IdentitySet new.
	control := #before.
	selector := #value.
	arguments := #().
	options := Set new.
	self parseOptions: self class defaultOptions.
]

{ #category : #installing }
MetaLink >> installOn: aNode [ 
	nodes add: aNode
	
]

{ #category : #'installing - link installer' }
MetaLink >> installOnClassVarNamed: aClassVarName for: aClassOrObject option: option instanceSpecific: instanceSpecific [
	| permalink |
	permalink := self permaLinkFor: aClassOrObject option: option instanceSpecific: instanceSpecific.
	^ self linkInstaller installPermaLink: permalink onClassVarNamed: aClassVarName
]

{ #category : #'installing - link installer' }
MetaLink >> installOnSlotNamed: aSlotName for: aClassOrObject option: option instanceSpecific: instanceSpecific [
	| permalink |
	permalink := self
		permaLinkFor: aClassOrObject
		option: option
		instanceSpecific: instanceSpecific.
	^ self linkInstaller installPermaLink: permalink onSlotNamed: aSlotName
]

{ #category : #'installing - link installer' }
MetaLink >> installOnTempVarNamed: aTempName inMethod: aMethodName for: aClassOrObject option: option instanceSpecific: instanceSpecific [
	| permalink |
	permalink := self permaLinkFor: aClassOrObject option: option instanceSpecific: instanceSpecific.
	^ self linkInstaller installPermaLink: permalink onTempVarNamed: aTempName inMethod: aMethodName
]

{ #category : #installing }
MetaLink >> installOnVariable: aVariable [
	nodes add: aVariable
]

{ #category : #installing }
MetaLink >> invalidate [
	nodes do: [ :node | node invalidate ]
]

{ #category : #accessing }
MetaLink >> level [
	<metaLinkOptions: #( + optionDisabledLink)>
	^ level
]

{ #category : #'link api' }
MetaLink >> level: anObject [
	level := anObject.
	self invalidate.
]

{ #category : #accessing }
MetaLink >> linkInstaller [
	^ self class linkInstaller
]

{ #category : #'installing - link installer' }
MetaLink >> linkToNode: aNode forObject: anObject [

	self linkInstaller install: self onNode: aNode forObject: anObject
]

{ #category : #accessing }
MetaLink >> metaLinkOptions: anArray [
	self options: anArray
]

{ #category : #accessing }
MetaLink >> metaObject [
	^ metaObject
]

{ #category : #'link api' }
MetaLink >> metaObject: anObject [
	metaObject := anObject.
	self optionInlineMetaObject ifTrue: [^self invalidate].
	nodes do: [ :node | | methodNode |
		methodNode := node methodNode.
		(methodNode metaLinkOptionsFromClassAndMethod includes: #optionInlineMetaObject)
			 ifTrue: [ methodNode method invalidate ] ].
	
	
]

{ #category : #accessing }
MetaLink >> methods [
	^nodes flatCollect: [ :entity |
		(entity isKindOf: RBProgramNode) 
			ifTrue: [ {entity methodNode compiledMethod}  ]
			ifFalse: [ entity usingMethods ] ]
]

{ #category : #accessing }
MetaLink >> nodes [
	^nodes
]

{ #category : #accessing }
MetaLink >> option: anArray [ 
	self parseOptions: anArray
]

{ #category : #options }
MetaLink >> optionAnnounce [
	^ options includes: #optionAnnounce
]

{ #category : #options }
MetaLink >> optionAnnounce: aBoolean [
	aBoolean
		ifTrue: [ options add: #optionAnnounce ] 
		ifFalse: [ options remove: #optionAnnounce ifAbsent:[] ]
]

{ #category : #options }
MetaLink >> optionArgsAsArray [
	^ options includes: #argsAsArray
]

{ #category : #options }
MetaLink >> optionArgsAsArray: aBoolean [
	"If this option is set to true, this metalink will send the required reified elements to its metaobject as one single array instead of as multiple arguments"
	aBoolean
		ifTrue: [ options add: #argsAsArray ] 
		ifFalse: [ options remove: #argsAsArray ifAbsent:[] ]
]

{ #category : #options }
MetaLink >> optionCompileOnLinkInstallation [
	^ options includes: #optionCompileOnLinkInstallation
]

{ #category : #options }
MetaLink >> optionCompileOnLinkInstallation: aBoolean [
	aBoolean
		ifTrue: [ options add: #optionCompileOnLinkInstallation ] 
		ifFalse: [ options remove: #optionCompileOnLinkInstallation ifAbsent:[] ]
]

{ #category : #options }
MetaLink >> optionDisabledLink [
	^ options includes: #optionDisabledLink
]

{ #category : #options }
MetaLink >> optionDisabledLink: aBoolean [
	aBoolean
		ifTrue: [ options add: #optionDisabledLink ] 
		ifFalse: [ options remove: #optionDisabledLink ifAbsent:[] ]
]

{ #category : #options }
MetaLink >> optionInlineCondition [
	^ options includes: #optionInlineCondition
]

{ #category : #options }
MetaLink >> optionInlineCondition: aBoolean [
	aBoolean
		ifTrue: [ options add: #optionInlineCondition ] 
		ifFalse: [ options remove: #optionInlineCondition ifAbsent:[] ]
]

{ #category : #options }
MetaLink >> optionInlineMetaObject [
	^ options includes: #optionInlineMetaObject
]

{ #category : #options }
MetaLink >> optionInlineMetaObject: aBoolean [
	aBoolean
		ifTrue: [ options add: #optionInlineMetaObject ] 
		ifFalse: [ options remove: #optionInlineMetaObject ifAbsent:[] ]
]

{ #category : #options }
MetaLink >> optionMetalevel [
	^ options includes: #optionMetalevel
]

{ #category : #options }
MetaLink >> optionMetalevel: aBoolean [
	aBoolean
		ifTrue: [ options add: #optionMetalevel ] 
		ifFalse: [ options remove: #optionMetalevel ifAbsent:[] ]
]

{ #category : #options }
MetaLink >> optionOneShot [
	^ options includes: #optionOneShot
]

{ #category : #options }
MetaLink >> optionOneShot: aBoolean [
	aBoolean
		ifTrue: [ options add: #optionOneShot ] 
		ifFalse: [ options remove: #optionOneShot ifAbsent:[] ]
]

{ #category : #accessing }
MetaLink >> options [
	^options
]

{ #category : #accessing }
MetaLink >> options: anArray [ 
	self parseOptions: anArray
]

{ #category : #private }
MetaLink >> parseOptions: optionsArray [

	options parseOptions: optionsArray
]

{ #category : #'installing - link installer' }
MetaLink >> permaLinkFor: aClassOrObject option: option instanceSpecific: instanceSpecific [
	| permaLink |
	permaLink := PermaLink new.
	permaLink persistenceType: option.
	permaLink isInstanceSpecific: instanceSpecific.
	permaLink slotOrVarClass: aClassOrObject.
	permaLink link: self.
	^ permaLink
]

{ #category : #'link api' }
MetaLink >> printString [
	|ws|
	ws := WriteStream on: String new.
	ws nextPutAll: (self control ifNil:['']).
	ws space.
	ws nextPutAll: metaObject printString.
	ws space.
	ws nextPutAll: selector asString.
	^ws contents
]

{ #category : #private }
MetaLink >> reificationsFor: aBlockOrBoolean [
	(aBlockOrBoolean isKindOf: Boolean) ifTrue: [ ^#() ].
	(aBlockOrBoolean isKindOf: RFCondition) ifTrue: [^aBlockOrBoolean reifications].
	^aBlockOrBoolean argumentNames.
]

{ #category : #installing }
MetaLink >> removeNode: aNode [
	"Silently remove aNode from the nodes list.
	No effect if aNode was not initially present."
	self hasNodes ifFalse:[^self].
	nodes remove: aNode ifAbsent:[]
]

{ #category : #accessing }
MetaLink >> selector [
	^selector
	
]

{ #category : #'link api' }
MetaLink >> selector: aSymbol [
	selector := aSymbol.
	self invalidate.
]

{ #category : #installing }
MetaLink >> uninstall [
	self linkInstaller uninstallFromAllAnonymousNodes: self.
	self linkInstaller uninstallPermaLinkFor: self.
	nodes do: [ :node | node removeLink: self ].
	nodes removeAll 
]

{ #category : #'installing - link installer' }
MetaLink >> unlinkFromNode: aNode forObject: anObject [

	self linkInstaller uninstall: self fromNode: aNode forObject: anObject
]

{ #category : #ast }
MetaLink >> valueInContext: aBlock [
	| process |
	"We need to take care that all code called here can not have active metalinks. We use copies of methods if we call system methods"
	process := Processor rfactiveProcess.
	(process isActive: self level)
		ifFalse: [ ^ self ].
	process shiftLevelUp.
	[aBlock value] rfEnsure: [process shiftLevelDown]
	
]

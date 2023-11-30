Extension { #name : 'Object' }

{ #category : '*Reflectivity' }
Object >> asLiteralNode [

	^ ASTLiteralNode value: self
]

{ #category : '*Reflectivity' }
Object >> asMethodConstant [

	| const constNode link |
	"this method will replace full sender message send of sender with me (or my value which usefull for block). So following sender method execution will not evaluate my receiver expresion but instead push precomputed literal with me"
	const := self value.
	constNode := thisContext sender sourceNodeExecuted.
	link := MetaLink new
		        metaObject: const;
		        control: #instead.
	constNode link: link.
	^ const
]

{ #category : '*Reflectivity' }
Object >> haltOnAccess [

	^ VariableBreakpoint watchVariablesInObject: self
]

{ #category : '*Reflectivity' }
Object >> haltOnAccessTo: aVariableName [

	^ VariableBreakpoint watchVariable: aVariableName inObject: self
]

{ #category : '*Reflectivity' }
Object >> haltOnCallTo: aSelector [

	^ Breakpoint breakOn: aSelector inObject: self
]

{ #category : '*Reflectivity' }
Object >> haltOnRead [

	^ VariableBreakpoint watchVariablesReadsInObject: self
]

{ #category : '*Reflectivity' }
Object >> haltOnReadTo: aVariableName [

	^ VariableBreakpoint watchVariableReads: aVariableName inObject: self
]

{ #category : '*Reflectivity' }
Object >> haltOnWrite [

	^ VariableBreakpoint watchVariablesWritesInObject: self
]

{ #category : '*Reflectivity' }
Object >> haltOnWriteTo: aVariableName [

	^ VariableBreakpoint
		  watchVariableWrites: aVariableName
		  inObject: self
]

{ #category : '*Reflectivity' }
Object >> haltOnceOnCallTo: aSelector [

	^ Breakpoint breakOnceOn: aSelector inObject: self
]

{ #category : '*Reflectivity' }
Object >> intanceSpecificMetaLinksAvailable [
	^true
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toAST: aNode [
	aNode link: aMetaLink forObject: self
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toClassVariable: aClassVariable [
	"Object specific links cannot be put on class variables"

	self shouldNotImplement
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toClassVariableNamed: aClassVariableName [
	self link: aMetaLink toClassVariableNamed: aClassVariableName option: #all
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toClassVariableNamed: aClassVariableName option: option [
	aMetaLink
		installOnVariableNamed: aClassVariableName
		for: self
		option: option
		instanceSpecific: self intanceSpecificMetaLinksAvailable
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toMethodNamed: aMethodName [
	self link: aMetaLink toAST: (self nonAnonymousClass lookupSelector: aMethodName asSymbol) ast
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toNodes: nodes [
	nodes do: [ :node | self link: aMetaLink toAST: node ]
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toSlot: aSlot [
	"Object specific links cannot be put on slots"

	self shouldNotImplement
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toSlotNamed: aSlotName [
	self link: aMetaLink toSlotNamed: aSlotName option: #all
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toSlotNamed: aSlotName option: option [
	aMetaLink
		installOnVariableNamed: aSlotName
		for: self
		option: option
		instanceSpecific: self intanceSpecificMetaLinksAvailable
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toTemporary: aTempVar [
	"Object specific links cannot be put on temporaries"

	self shouldNotImplement
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toTemporaryNamed: aTempVarName inMethod: aMethodName [
	self
		link: aMetaLink
		toTemporaryNamed: aTempVarName
		inMethod: aMethodName
		option: #all
]

{ #category : '*Reflectivity' }
Object >> link: aMetaLink toTemporaryNamed: aTempVarName inMethod: aMethodName option: option [
	| variable |

	variable := (self nonAnonymousClass lookupSelector: aMethodName) lookupVar: aTempVarName.

	aMetaLink
		installOnVariable: variable
		for: self
		option: option
		instanceSpecific: true
]

Extension { #name : #Object }

{ #category : #'*Reflectivity' }
Object >> asLiteralNode [
	^RBLiteralNode value: self
]

{ #category : #'*Reflectivity' }
Object >> asMethodConstant [
	| const constNode link |
	"this method will replace full sender message send of sender with me (or my value which usefull for block). So following sender method execution will not evaluate my receiver expresion but instead push precomputed literal with me"
	const := self value.
	constNode := thisContext sender sourceNodeExecuted.
	link := MetaLink new
		metaObject: const;
		control: #instead.
	constNode link: link.
	^const
]

{ #category : #'*Reflectivity' }
Object >> intanceSpecificMetaLinksAvailable [
	^true
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toAST: aNode [
	aNode link: aMetaLink forObject: self
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toClassVariable: aClassVariable [
	"Object specific links cannot be put on class variables"

	self shouldNotImplement
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toClassVariableNamed: aClassVariableName [
	self link: aMetaLink toClassVariableNamed: aClassVariableName option: #all
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toClassVariableNamed: aClassVariableName option: option [
	aMetaLink
		installOnClassVarNamed: aClassVariableName
		for: self
		option: option
		instanceSpecific: self intanceSpecificMetaLinksAvailable
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toMethodNamed: aMethodName [
	self link: aMetaLink toAST: (MetalinkTargetResolver methodNamed: aMethodName for: self)
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toNodes: nodes [
	nodes do: [ :node | self link: aMetaLink toAST: node ]
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toSlot: aSlot [
	"Object specific links cannot be put on slots"

	self shouldNotImplement
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toSlotNamed: aSlotName [
	self link: aMetaLink toSlotNamed: aSlotName option: #all
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toSlotNamed: aSlotName option: option [
	aMetaLink
		installOnSlotNamed: aSlotName
		for: self
		option: option
		instanceSpecific: self intanceSpecificMetaLinksAvailable
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toTemporary: aTempVar [
	"Object specific links cannot be put on temporaries"

	self shouldNotImplement
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toTemporaryNamed: aTempVarName inMethod: aMethodName [
	self
		link: aMetaLink
		toTemporaryNamed: aTempVarName
		inMethod: aMethodName
		option: #all
]

{ #category : #'*Reflectivity' }
Object >> link: aMetaLink toTemporaryNamed: aTempVarName inMethod: aMethodName option: option [
	aMetaLink
		installOnTempVarNamed: aTempVarName
		inMethod: aMethodName
		for: self
		option: option
		instanceSpecific: self intanceSpecificMetaLinksAvailable
]

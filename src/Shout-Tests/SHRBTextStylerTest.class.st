Class {
	#name : 'SHRBTextStylerTest',
	#superclass : 'TestCase',
	#instVars : [
		'styler',
		'oldSetting'
	],
	#category : 'Shout-Tests-Styling',
	#package : 'Shout-Tests',
	#tag : 'Styling'
}

{ #category : 'running' }
SHRBTextStylerTest >> setUp [

	super setUp.
	styler := SHRBTextStyler new.
	oldSetting := SHRBTextStyler instVarNamed: #formatIncompleteIdentifiers
]

{ #category : 'private - utilities' }
SHRBTextStylerTest >> style: aText [

	| ast |

	ast := self class compiler parse: aText asString.
	styler style: aText ast: ast.

	^ ast
]

{ #category : 'running' }
SHRBTextStylerTest >> tearDown [

	SHRBTextStyler formatIncompleteIdentifiers: oldSetting.
	super tearDown
]

{ #category : 'tests' }
SHRBTextStylerTest >> testClassAfterMessageToASymbolLowercaseShouldBeColored [

	| aText index attributes |

	SHRBTextStyler formatIncompleteIdentifiers: true.

	aText := 'm1
		#world traceCr.
		Object new.' asText.

	index := aText string indexOfSubCollection: 'Object'.

	self style: aText.

	attributes := aText attributesAt: index.

	self assertCollection: attributes hasSameElements: {
		TextClassLink className: #Object.
		TextColor color: SHPreferences globalVarStyle color }
]

{ #category : 'tests' }
SHRBTextStylerTest >> testClassAfterMessageToASymbolUppercaseShouldBeColored [

	| aText index attributes |

	SHRBTextStyler formatIncompleteIdentifiers: true.

	aText := 'm1
		#World traceCr.
		Object new.' asText.

	index := aText string indexOfSubCollection: 'Object'.

	self style: aText.

	attributes := aText attributesAt: index.

	self assertCollection: attributes hasSameElements: {
		TextClassLink className: #Object.
		TextColor color: SHPreferences globalVarStyle color}
]

{ #category : 'tests' }
SHRBTextStylerTest >> testIncompleteIdentifierVariable [

	| aText ast |

	aText := 'm1
		^ sel' asText.

	ast := self style: aText.
	"here the identifier is incomplete"
	self assert: (styler resolveStyleFor: ast variableNodes first) equals: #incompleteIdentifier.

	aText := 'm1
		^ asdfasf' asText.

	ast := self style: aText.
	"But for a name that does not exist at all, it is #undefinedIdentifier"
	self assert: (styler resolveStyleFor: ast variableNodes first) equals: #undefinedIdentifier
]

{ #category : 'tests' }
SHRBTextStylerTest >> testSettingFalseTheIncompleteMessageShouldNotFormatIt [

	| aText index attributes ast node |

	SHRBTextStyler formatIncompleteIdentifiers: false.

	aText := 'm1
		^ self prin' asText.

	index := aText string indexOfSubCollection: 'prin'.

	ast := self style: aText.

	attributes := aText attributesAt: index.

	node := ast allChildren detect: [:e | e isMessage ].

	self assertCollection: attributes hasSameElements: {
		TextMethodLink sourceNode: node.
		TextColor red }
]

{ #category : 'tests' }
SHRBTextStylerTest >> testSettingTrueTheIncompleteMessageShouldFormatIt [

	| aText index attributes ast node |

	SHRBTextStyler formatIncompleteIdentifiers: true.

	aText := 'm1
		^ self prin' asText.

	index := aText string indexOfSubCollection: 'prin'.

	ast := self style: aText.

	attributes := aText attributesAt: index.

	node := ast allChildren detect: [:e | e isMessage ].

	self assertCollection: attributes hasSameElements: {
		TextMethodLink sourceNode: node.
		TextEmphasis italic }
]

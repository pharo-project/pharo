Class {
	#name : #SHRBTextStylerTest,
	#superclass : #TestCase,
	#instVars : [
		'styler',
		'oldSetting'
	],
	#category : #'Shout-Tests'
}

{ #category : #running }
SHRBTextStylerTest >> setUp [

	super setUp.
	styler := SHRBTextStyler new.
	oldSetting := SHRBTextStyler instVarNamed: #formatIncompleteIdentifiers.
]

{ #category : #running }
SHRBTextStylerTest >> style: aText [

	| ast |
	
	ast := self class compiler
		source: aText asString;
		noPattern: false ;
		options:  #(+ optionParseErrors + optionSkipSemanticWarnings);
		requestor: nil;
		parse.				
	ast doSemanticAnalysis.

	styler style: aText ast: ast.
	
	^ ast
]

{ #category : #running }
SHRBTextStylerTest >> tearDown [ 

	SHRBTextStyler formatIncompleteIdentifiers: oldSetting.
	super tearDown.
	
]

{ #category : #running }
SHRBTextStylerTest >> testSettingFalseTheIncompleteIdentifiersShouldNotFormatIt [
	
	| aText index attributes |

	SHRBTextStyler formatIncompleteIdentifiers: false.

	aText := 'm1
		^ se' asText.

	index := aText string indexOfSubCollection: 'se'.
	
	self style: aText.
	
	attributes := aText attributesAt: index.
	
	self assertCollection: attributes hasSameElements: { TextColor red. TextVariableLink variableName: 'se' }.
]

{ #category : #running }
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
		TextColor red }.
]

{ #category : #running }
SHRBTextStylerTest >> testSettingTrueTheIncompleteIdentifiersShouldFormatIt [
	
	| aText index attributes |

	SHRBTextStyler formatIncompleteIdentifiers: true.

	aText := 'm1
		^ se' asText.

	index := aText string indexOfSubCollection: 'se'.
	
	self style: aText.
	
	attributes := aText attributesAt: index.
	
	self assertCollection: attributes hasSameElements: { TextColor blue. TextEmphasis italic. TextVariableLink variableName: 'se' }.
]

{ #category : #running }
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
		TextEmphasis italic }.
]

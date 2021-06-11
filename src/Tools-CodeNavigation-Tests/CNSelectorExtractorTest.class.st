"
A CNSelectorExtractorTest is a test class for testing the behavior of CNSelectorExtractor
"
Class {
	#name : #CNSelectorExtractorTest,
	#superclass : #TestCase,
	#classVars : [
		'selectorExtractor'
	],
	#category : #'Tools-CodeNavigation-Tests'
}

{ #category : #initialization }
CNSelectorExtractorTest >> initialize [
			
	selectorExtractor := CNSelectorExtractor new.
]

{ #category : #tests }
CNSelectorExtractorTest >> testExtractSelectorFromSourceAtPositionBorderCasesReturnsNil [

   |source position|
	"Border cases first"
	
	"Nil source, returns nil"
	source := nil.
	position := 1.
	self assert: (selectorExtractor extractSelectorFromSource: source atPosition: position) isNil.
	
	"Empty source, returns nil"
	source := ''.
	position := 1.
	self assert: (selectorExtractor extractSelectorFromSource: source atPosition: position) isNil.
	
	"nil source, invalid Position"
	source := nil.
	position := -1.
	self assert: (selectorExtractor extractSelectorFromSource: source atPosition: position) isNil.
	
	"a string source, invalid Position"
	source := 'astring'.
	position := 10000.
	self assert: (selectorExtractor extractSelectorFromSource: source atPosition: position) isNil.

]

{ #category : #tests }
CNSelectorExtractorTest >> testExtractSelectorFromSourceAtPositionGeneralCasesExtractsSelector [

	"Note that the tested method (#extractSelectorFromSource:atPosition:) uses internally #extractSelectorFromAST:atPosition:"
	"Also uses RBParser >> #parseFaultyExpression: "
	"This means, that by covering only the border cases, there is full code coverage, assumming extractSelectorFromAST:atPosition: and RBParser >> #parseFaultyExpression:  tests are ok."

	| source position |
	"Empty source, invalid position, returns nil"
	source := ''.
	position := -1.
	self assert: (selectorExtractor
			 extractSelectorFromSource: source
			 atPosition: position) isNil.

	"Cascade"
	source := 'foo bar: 7; fum'.
	position := 1.
	self
		assert: (selectorExtractor
				 extractSelectorFromSource: source
				 atPosition: position)
		equals: #bar:.

	position := 12.
	self
		assert: (selectorExtractor
				 extractSelectorFromSource: source
				 atPosition: position)
		equals: #fum.

	"A good case of IfTrue if false. Should select #ifTrue:ifFalse:"
	source := 'self isInteger ifTrue: [ #(1 2) first. self printString ] ifFalse: [ self yourself ]'.
	position := source indexOfSubCollection: 'alse:'.
	self
		assert: (selectorExtractor
				 extractSelectorFromSource: source
				 atPosition: position)
		equals: #ifTrue:ifFalse:.

	"A good case of nesting. Should select #first"
	source := 'self isInteger ifTrue: [ #(1 2) first. self printString ] ifFalse: [ self yourself ]'.
	position := source indexOfSubCollection: 'irst'.
	self
		assert: (selectorExtractor
				 extractSelectorFromSource: source
				 atPosition: position)
		equals: #first
]

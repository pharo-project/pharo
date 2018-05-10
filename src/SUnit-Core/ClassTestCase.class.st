"
This class is intended for unit tests of individual classes and their metaclasses.

It provides methods to determine the coverage of the unit tests.

Subclasses are expected to re-implement #classesToBeTested and #selectorsToBeIgnored.

They should also implement to confirm that all methods have been tested.

#testCoverage

	super testCoverage.


"
Class {
	#name : #ClassTestCase,
	#superclass : #TestCase,
	#category : #'SUnit-Core-Utilities'
}

{ #category : #testing }
ClassTestCase class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self name = #ClassTestCase
			
]

{ #category : #testing }
ClassTestCase class >> mustTestCoverage [

	^ false
]

{ #category : #private }
ClassTestCase >> categoriesForClass: aClass [

 ^ aClass organization allMethodSelectors collect: 
			[:each |  aClass organization categoryOfElement: each].

]

{ #category : #coverage }
ClassTestCase >> classToBeTested [
	
	^ self subclassResponsibility
]

{ #category : #utilities }
ClassTestCase >> differentMethodsWithSameSelectorBetween: firstClass and: secondClass [
	| repeatedSelectors differentMethodsWithSameSelector |
	
	repeatedSelectors := firstClass localSelectors intersection: secondClass localSelectors.
	
	differentMethodsWithSameSelector := repeatedSelectors select: [ :selector | | m1 m2| 
										 m1 := firstClass>>selector. 
										 m2 := secondClass>>selector.
										 m1 sourceCode ~= m2 sourceCode].
									
	^differentMethodsWithSameSelector.
]

{ #category : #utilities }
ClassTestCase >> repeatedMethodsThatDoNotAccessInstanceVariablesBetween: firstClass and: secondClass [
	| repeatedSelectors repeatedMethodsThatDoNotAccessInstanceVariables |
	
	repeatedSelectors := firstClass localSelectors intersection: secondClass localSelectors.
	
	repeatedMethodsThatDoNotAccessInstanceVariables := repeatedSelectors select: [ :selector | | m1 m2| 
										 m1 := firstClass>>selector. 
										 m2 := secondClass>>selector.
										((m1 sourceCode = m2 sourceCode) and: [ m1 hasInstVarRef not ]) and: [ m2 hasInstVarRef not ]].
									
	^repeatedMethodsThatDoNotAccessInstanceVariables.
]

{ #category : #coverage }
ClassTestCase >> selectorsNotTested [

	^ self selectorsToBeTested difference: self selectorsTested.

]

{ #category : #coverage }
ClassTestCase >> selectorsTested [
	| literals |
	literals := Set new.
	self class
		selectorsAndMethodsDo: [ :s :m | (s beginsWith: 'test')
			ifTrue: [ literals addAll: (m messages)] ].
	^ literals asArray sort
]

{ #category : #coverage }
ClassTestCase >> selectorsToBeIgnored [
	^ #()
]

{ #category : #coverage }
ClassTestCase >> selectorsToBeTested [

	^ ( { self classToBeTested. self classToBeTested class } flatCollect: [:c | c selectors]) 
			difference: self selectorsToBeIgnored
]

{ #category : #private }
ClassTestCase >> targetClass [
	[ ^ self classToBeTested ]
		on: Error
		do: [ 
			| className |
			className := self class name asString copyFrom: 1 to: self class name size - 4.
			^ Smalltalk globals at: className asString asSymbol ]
]

{ #category : #tests }
ClassTestCase >> testClassComment [
	self assert: self targetClass organization hasComment.
]

{ #category : #tests }
ClassTestCase >> testCoverage [

	| untested | 
	self class mustTestCoverage ifTrue:
		[ untested := self selectorsNotTested.
		self assert: untested isEmpty 
		description: untested size asString, ' selectors are not covered' ]
]

{ #category : #tests }
ClassTestCase >> testNew [
	self targetClass new
]

{ #category : #tests }
ClassTestCase >> testUnCategorizedMethods [
	| uncategorizedMethods  |
	uncategorizedMethods := self targetClass selectorsInProtocol: Protocol unclassified.
	self 
		assert: uncategorizedMethods isEmpty
		description: uncategorizedMethods asString	
]

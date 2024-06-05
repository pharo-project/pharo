"
should: will be deprecated sooner or leater. Use assert: instead
"
Class {
	#name : 'ReShouldTransformedIntoAssertRule',
	#superclass : 'ReNodeRewriteRule',
	#traits : 'ReTSUnitGroupedRule',
	#classTraits : 'ReTSUnitGroupedRule classTrait',
	#category : 'SUnit-Rules-SUnit',
	#package : 'SUnit-Rules',
	#tag : 'SUnit'
}

{ #category : 'accessing' }
ReShouldTransformedIntoAssertRule class >> group [

	^ 'SUnit'
]

{ #category : 'accessing' }
ReShouldTransformedIntoAssertRule class >> ruleName [
	^ 'Use assert: instead of should:'
]

{ #category : 'accessing' }
ReShouldTransformedIntoAssertRule class >> uniqueIdentifierName [

	^'ShouldTransformedIntoAssert'
]

{ #category : 'initialization' }
ReShouldTransformedIntoAssertRule >> initialize [
	super initialize.

	self
		replace: 'self should: [ ``@object1 ]'
		with: 'self assert: ``@object1'
]

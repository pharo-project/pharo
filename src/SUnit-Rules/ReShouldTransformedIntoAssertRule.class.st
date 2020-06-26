"
should: will be deprecated sooner or leater. Use assert: instead
"
Class {
	#name : #ReShouldTransformedIntoAssertRule,
	#superclass : #ReNodeRewriteRule,
	#traits : 'ReTSUnitGroupedRule',
	#classTraits : 'ReTSUnitGroupedRule classTrait',
	#category : #'SUnit-Rules-Base'
}

{ #category : #accessing }
ReShouldTransformedIntoAssertRule class >> uniqueIdentifierName [
	
	^'ShouldTransformedIntoAssert'
]

{ #category : #initialization }
ReShouldTransformedIntoAssertRule >> initialize [
	super initialize.

	self
		replace: 'self should: [ ``@object1 ]'
		with: 'self assert: ``@object1'
]

{ #category : #accessing }
ReShouldTransformedIntoAssertRule >> name [

	^ 'Use assert: instead of should:'
]

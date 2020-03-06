"
should: will be deprecated sooner or leater. Use assert: instead
"
Class {
	#name : #ReShouldTransformedIntoAssertRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'SUnit-Rules'
}

{ #category : #accessing }
ReShouldTransformedIntoAssertRule class >> uniqueIdentifierName [
	
	^'ShouldTransformedIntoAssert'
]

{ #category : #accessing }
ReShouldTransformedIntoAssertRule >> group [

	^ 'SUnit'
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

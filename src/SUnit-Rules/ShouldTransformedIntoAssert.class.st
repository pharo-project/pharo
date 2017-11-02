"
should: will be deprecated sooner or leater. Use assert: instead
"
Class {
	#name : #ShouldTransformedIntoAssert,
	#superclass : #ReNodeRewriteRule,
	#category : 'SUnit-Rules'
}

{ #category : #accessing }
ShouldTransformedIntoAssert class >> uniqueIdentifierName [
	
	^'ShouldTransformedIntoAssert'
]

{ #category : #accessing }
ShouldTransformedIntoAssert >> group [

	^ 'SUnit'
]

{ #category : #initialization }
ShouldTransformedIntoAssert >> initialize [
	super initialize.

	self
		replace: 'self should: [ ``@object1 ]'
		with: 'self assert: ``@object1'
]

{ #category : #accessing }
ShouldTransformedIntoAssert >> name [

	^ 'Use assert: instead of should:'
]

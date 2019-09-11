Class {
	#name : #RBDummyCompositeLintRuleTest,
	#superclass : #RBDummyLintRuleTest,
	#instVars : [
		'rules'
	],
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #'instance creation' }
RBDummyCompositeLintRuleTest class >> allRules [
	^self ruleFor: self protocol: 'all checks'
]

{ #category : #'all checks' }
RBDummyCompositeLintRuleTest class >> lintChecks [
	^ self 
		rules: (RBBasicLintRuleTestData protocolsToCheck collect: 
			[ :each | 
			self 
				ruleFor: RBBasicLintRuleTestData
				protocol: each ])
		name: 'Lint checks'
]

{ #category : #'instance creation' }
RBDummyCompositeLintRuleTest class >> ruleFor: aClass protocol: aProtocol [ 
	^self
		rules: (((RBBrowserEnvironment new selectorsFor: aProtocol asSymbol in: aClass class)
				collect: [:selector | aClass perform: selector])
					asSortedCollection: [:a :b | a name < b name])
		name: ((aProtocol asString copy) at: 1 put: aProtocol first asUppercase;
				yourself)
]

{ #category : #'instance creation' }
RBDummyCompositeLintRuleTest class >> rules: aCollection [ 
	^self new rules: aCollection
]

{ #category : #'instance creation' }
RBDummyCompositeLintRuleTest class >> rules: aCollection name: aString [ 
	^(self new) rules: aCollection;
		name: aString;
		yourself
]

{ #category : #'all checks' }
RBDummyCompositeLintRuleTest class >> transformations [
	^ self 
		ruleFor: RBTransformationRuleTestData
		protocol: 'transformations'
]

{ #category : #accessing }
RBDummyCompositeLintRuleTest >> checkClass: aSmalllintContext [ 
	rules do: 
			[:each | 
			each checkClass: aSmalllintContext.
			Processor yield]
]

{ #category : #accessing }
RBDummyCompositeLintRuleTest >> checkMethod: aSmalllintContext [ 
	rules do: 
			[:each | 
			each checkMethod: aSmalllintContext.
			Processor yield]
]

{ #category : #accessing }
RBDummyCompositeLintRuleTest >> failedRules [
	^rules inject: OrderedCollection new into: [:oc :each | oc addAll: each failedRules; yourself]
]

{ #category : #testing }
RBDummyCompositeLintRuleTest >> hasConflicts [
	^(rules detect: [:each | each hasConflicts] ifNone: [nil]) notNil
]

{ #category : #testing }
RBDummyCompositeLintRuleTest >> isComposite [
	^true
]

{ #category : #testing }
RBDummyCompositeLintRuleTest >> isEmpty [
	^(rules detect: [:each | each isEmpty not] ifNone: [nil]) isNil
]

{ #category : #accessing }
RBDummyCompositeLintRuleTest >> problemCount [
	^rules inject: 0 into: [:count :each | count + each problemCount]
]

{ #category : #initialization }
RBDummyCompositeLintRuleTest >> resetResult [
	rules do: [:each | each resetResult]
]

{ #category : #accessing }
RBDummyCompositeLintRuleTest >> rules [
	^rules
]

{ #category : #initialization }
RBDummyCompositeLintRuleTest >> rules: aCollection [
	rules := aCollection
]

{ #category : #private }
RBDummyCompositeLintRuleTest >> viewResults [
	rules do: [:each | each viewResults]
]

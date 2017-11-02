Class {
	#name : #RBCompositeLintRuleTest,
	#superclass : #RBLintRuleTest,
	#instVars : [
		'rules'
	],
	#category : 'Refactoring-Tests-Core-Data'
}

{ #category : #'instance creation' }
RBCompositeLintRuleTest class >> allRules [
	^self ruleFor: self protocol: 'all checks'
]

{ #category : #'all checks' }
RBCompositeLintRuleTest class >> lintChecks [
	^ self 
		rules: (RBBasicLintRuleTest protocolsToCheck collect: 
			[ :each | 
			self 
				ruleFor: RBBasicLintRuleTest
				protocol: each ])
		name: 'Lint checks'
]

{ #category : #'instance creation' }
RBCompositeLintRuleTest class >> ruleFor: aClass protocol: aProtocol [ 
	^self
		rules: (((RBBrowserEnvironment new selectorsFor: aProtocol asSymbol in: aClass class)
				collect: [:selector | aClass perform: selector])
					asSortedCollection: [:a :b | a name < b name])
		name: ((aProtocol asString copy) at: 1 put: aProtocol first asUppercase;
				yourself)
]

{ #category : #'instance creation' }
RBCompositeLintRuleTest class >> rules: aCollection [ 
	^self new rules: aCollection
]

{ #category : #'instance creation' }
RBCompositeLintRuleTest class >> rules: aCollection name: aString [ 
	^(self new) rules: aCollection;
		name: aString;
		yourself
]

{ #category : #'all checks' }
RBCompositeLintRuleTest class >> transformations [
	^ self 
		ruleFor: RBTransformationRuleTest
		protocol: 'transformations'
]

{ #category : #accessing }
RBCompositeLintRuleTest >> checkClass: aSmalllintContext [ 
	rules do: 
			[:each | 
			each checkClass: aSmalllintContext.
			Processor yield]
]

{ #category : #accessing }
RBCompositeLintRuleTest >> checkMethod: aSmalllintContext [ 
	rules do: 
			[:each | 
			each checkMethod: aSmalllintContext.
			Processor yield]
]

{ #category : #accessing }
RBCompositeLintRuleTest >> failedRules [
	^rules inject: OrderedCollection new into: [:oc :each | oc addAll: each failedRules; yourself]
]

{ #category : #testing }
RBCompositeLintRuleTest >> hasConflicts [
	^(rules detect: [:each | each hasConflicts] ifNone: [nil]) notNil
]

{ #category : #testing }
RBCompositeLintRuleTest >> isComposite [
	^true
]

{ #category : #testing }
RBCompositeLintRuleTest >> isEmpty [
	^(rules detect: [:each | each isEmpty not] ifNone: [nil]) isNil
]

{ #category : #accessing }
RBCompositeLintRuleTest >> problemCount [
	^rules inject: 0 into: [:count :each | count + each problemCount]
]

{ #category : #initialize-release }
RBCompositeLintRuleTest >> resetResult [
	rules do: [:each | each resetResult]
]

{ #category : #accessing }
RBCompositeLintRuleTest >> rules [
	^rules
]

{ #category : #initialize-release }
RBCompositeLintRuleTest >> rules: aCollection [
	rules := aCollection
]

{ #category : #private }
RBCompositeLintRuleTest >> viewResults [
	rules do: [:each | each viewResults]
]

Class {
	#name : #RBCompositeLintRuleTestData,
	#superclass : #RBLintRuleTestData,
	#instVars : [
		'rules'
	],
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #'instance creation' }
RBCompositeLintRuleTestData class >> allRules [
	^self ruleFor: self protocol: 'all checks'
]

{ #category : #'all checks' }
RBCompositeLintRuleTestData class >> lintChecks [
	^ self 
		rules: (RBBasicLintRuleTestData protocolsToCheck collect: 
			[ :each | 
			self 
				ruleFor: RBBasicLintRuleTestData
				protocol: each ])
		name: 'Lint checks'
]

{ #category : #'instance creation' }
RBCompositeLintRuleTestData class >> ruleFor: aClass protocol: aProtocol [ 
	^self
		rules: (((RBBrowserEnvironment new selectorsFor: aProtocol asSymbol in: aClass class)
				collect: [:selector | aClass perform: selector])
					asSortedCollection: [:a :b | a name < b name])
		name: ((aProtocol asString copy) at: 1 put: aProtocol first asUppercase;
				yourself)
]

{ #category : #'instance creation' }
RBCompositeLintRuleTestData class >> rules: aCollection [ 
	^self new rules: aCollection
]

{ #category : #'instance creation' }
RBCompositeLintRuleTestData class >> rules: aCollection name: aString [ 
	^(self new) rules: aCollection;
		name: aString;
		yourself
]

{ #category : #'all checks' }
RBCompositeLintRuleTestData class >> transformations [
	^ self 
		ruleFor: RBTransformationRuleTestData
		protocol: 'transformations'
]

{ #category : #accessing }
RBCompositeLintRuleTestData >> checkClass: aSmalllintContext [ 
	rules do: 
			[:each | 
			each checkClass: aSmalllintContext.
			Processor yield]
]

{ #category : #accessing }
RBCompositeLintRuleTestData >> checkMethod: aSmalllintContext [ 
	rules do: 
			[:each | 
			each checkMethod: aSmalllintContext.
			Processor yield]
]

{ #category : #accessing }
RBCompositeLintRuleTestData >> failedRules [
	^rules inject: OrderedCollection new into: [:oc :each | oc addAll: each failedRules; yourself]
]

{ #category : #testing }
RBCompositeLintRuleTestData >> hasConflicts [
	^ rules anySatisfy: [ :each | each hasConflicts ]
]

{ #category : #testing }
RBCompositeLintRuleTestData >> isComposite [
	^true
]

{ #category : #testing }
RBCompositeLintRuleTestData >> isEmpty [
	^ rules allSatisfy: [ :each | each isEmpty ]
]

{ #category : #accessing }
RBCompositeLintRuleTestData >> problemCount [
	^rules inject: 0 into: [:count :each | count + each problemCount]
]

{ #category : #initialization }
RBCompositeLintRuleTestData >> resetResult [
	rules do: [:each | each resetResult]
]

{ #category : #accessing }
RBCompositeLintRuleTestData >> rules [
	^rules
]

{ #category : #initialization }
RBCompositeLintRuleTestData >> rules: aCollection [
	rules := aCollection
]

{ #category : #private }
RBCompositeLintRuleTestData >> viewResults [
	rules do: [:each | each viewResults]
]

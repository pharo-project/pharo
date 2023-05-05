"
I am a test case ensuring that some protocols that were removed from the system are not introduced again. 

The goal is to keep some conventions around protocols. For example we should use #removing and not #remove or #removal. The goal is to keep the number of protocols in the default image resonable to be able to give better propositions to the users.
"
Class {
	#name : #ProtocolConventionsTest,
	#superclass : #TestCase,
	#category : #'ReleaseTests-Categorization'
}

{ #category : #asserting }
ProtocolConventionsTest >> assertProtocolName: aProtocolName notAcceptingProtocolNamesLike: aCollectionOfSelectors [
	| violations |
	violations := self class environment allClassesAndTraits
		select: [ :c | (c protocolNames includesAny: aCollectionOfSelectors) or: [ c class protocols includesAny: aCollectionOfSelectors ] ].
	self
		assert: violations isEmpty
		description: [ 'In the default Pharo images, the protocol #{1} should be used instead of {2}.
Some classes are violating this convention:
{3}'
				format:
					{aProtocolName . aCollectionOfSelectors asString . (String
						streamContents: [ :s |
							violations
								do: [ :c |
									s
										<< '- ';
										print: c ]
								separatedBy: [ s cr ] ])} ]
]

{ #category : #asserting }
ProtocolConventionsTest >> assertProtocolName: aProtocolName notAcceptingProtocolNamesLike: aCollectionOfSelectors notMoreViolationsThan: aCount [

	| violations |
	violations := self class environment allClassesAndTraits select: [ :c |
		              (c protocolNames includesAny: aCollectionOfSelectors) or: [ c class protocolNames includesAny: aCollectionOfSelectors ] ].
	self assert: violations size <= aCount description: [
		'In the default Pharo images, the protocol #{1} should be used instead of {2}.
Some classes are violating this convention:
{3}' format: {
				aProtocolName.
				aCollectionOfSelectors asString.
				(String streamContents: [ :s |
					 violations
						 do: [ :c |
							 s
								 << '- ';
								 print: c ]
						 separatedBy: [ s cr ] ]) } ]
]

{ #category : #utilities }
ProtocolConventionsTest >> checkIdiomForRuleClass: aRuleClass [

	self checkIdiomForRuleClass: aRuleClass notMoreViolationsThan: 0
]

{ #category : #utilities }
ProtocolConventionsTest >> checkIdiomForRuleClass: aRuleClass notMoreViolationsThan: aCount [

	| idiom |
	idiom := aRuleClass protocolIdiom.

	self
		assertProtocolName: idiom key
		notAcceptingProtocolNamesLike: idiom value
		notMoreViolationsThan: aCount
]

{ #category : #tests }
ProtocolConventionsTest >> testProperAccessingProtocolIsUsed [

	self checkIdiomForRuleClass: ReProperMethodProtocolNameForAccessingRule
]

{ #category : #tests }
ProtocolConventionsTest >> testProperAddingRemovingProtocolIsUsed [

	self checkIdiomForRuleClass: ReProperMethodProtocolNameForAddingRemovingRule notMoreViolationsThan: 5
]

{ #category : #tests }
ProtocolConventionsTest >> testProperConvertingProtocolIsUsed [

	self checkIdiomForRuleClass: ReProperMethodProtocolNameForConvertingRule notMoreViolationsThan: 29
]

{ #category : #tests }
ProtocolConventionsTest >> testProperFileInOutProtocolIsUsed [

	self checkIdiomForRuleClass: ReProperMethodProtocolNameForFileInOutRule
]

{ #category : #tests }
ProtocolConventionsTest >> testProperInstanceCreationProtocolIsUsed [

	self checkIdiomForRuleClass: ReProperMethodProtocolNameForInstanceCreationRule
]

{ #category : #tests }
ProtocolConventionsTest >> testProperRemovalProtocolIsUsed [

	self checkIdiomForRuleClass: ReProperMethodProtocolNameForRemovalRule
]

{ #category : #tests }
ProtocolConventionsTest >> testProperTestProtocolIsUsed [

	self checkIdiomForRuleClass: ReProperMethodProtocolNameForTestsRule notMoreViolationsThan: 3
]

{ #category : #tests }
ProtocolConventionsTest >> testProperUtilitiesProtocolIsUsed [

	self checkIdiomForRuleClass: ReProperMethodProtocolNameForUtilitiesRule notMoreViolationsThan: 5
]

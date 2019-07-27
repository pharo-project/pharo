Class {
	#name : #ProtocolConventionsTest,
	#superclass : #TestCase,
	#category : #'ReleaseTests-Categorization'
}

{ #category : #asserting }
ProtocolConventionsTest >> assertProtocol: aCollectionOfSelectors areNotUsedInsteadOf: aProtocolName [
	| violations |
	violations := self class environment allClasses select: [ :c | c protocols includesAny: aCollectionOfSelectors ].
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

{ #category : #tests }
ProtocolConventionsTest >> testRemovalProtocolIsUsed [
	self assertProtocol: #(#remove #removal) areNotUsedInsteadOf: #removing
]

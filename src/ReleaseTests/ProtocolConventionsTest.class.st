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

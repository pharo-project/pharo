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
ProtocolConventionsTest >> assertProtocols: aCollectionOfSelectors areNotUsedInsteadOf: aProtocolName [
	| violations |
	violations := self class environment allClassesAndTraits
		select: [ :c | (c protocols includesAny: aCollectionOfSelectors) or: [ c class protocols includesAny: aCollectionOfSelectors ] ].
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
ProtocolConventionsTest >> testFileInOutProtocolIsUsed [
	self assertProtocols: #(#'filein/out' #'fileIn/Out' #'fileIn/out' #'filein/Out') areNotUsedInsteadOf: #'file in/out'
]

{ #category : #tests }
ProtocolConventionsTest >> testInstanceCreationProtocolIsUsed [
	self assertProtocols: #(#'instance-creation') areNotUsedInsteadOf: #'instance creation'
]

{ #category : #tests }
ProtocolConventionsTest >> testRemovalProtocolIsUsed [
	self assertProtocols: #(#remove #removal) areNotUsedInsteadOf: #removing
]

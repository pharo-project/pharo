tests - occurrencesOf for identity collections
testOccurencesOfForIdentityCollections

| collection element elementCopy |
collection := self identityCollectionWithElementsCopyNotIdentical .
element := collection anyOne.
elementCopy := element copy.

self assert: (collection occurrencesOf: elementCopy ) = 0.
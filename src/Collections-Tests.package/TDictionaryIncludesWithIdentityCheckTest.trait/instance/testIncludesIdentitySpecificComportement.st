tests - dictionary including
testIncludesIdentitySpecificComportement

| valueIn collection |
collection := self nonEmptyWithCopyNonIdentical  .
valueIn := collection  values anyOne.

self assert: (collection includesIdentity: valueIn ) .
self deny: (collection includesIdentity: valueIn copy ) .

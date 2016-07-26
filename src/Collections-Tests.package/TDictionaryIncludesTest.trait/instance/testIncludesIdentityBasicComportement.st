tests - dictionary including
testIncludesIdentityBasicComportement

| valueIn collection |
collection := self nonEmpty .
valueIn := collection  values anyOne.

self assert: (collection includesIdentity: valueIn ) .
self deny: (collection includesIdentity: self valueNotInNonEmpty ).
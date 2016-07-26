tests - literal specific behavior
testIncludesWithEqualElementFromDifferentClasses

| dict |
dict := self classToBeTested new.

dict at: 1 put: 'element1'.
dict at: #key put: 1.0.

self deny: (dict includesKey: 1.0).
self assert: (dict includes: 1)
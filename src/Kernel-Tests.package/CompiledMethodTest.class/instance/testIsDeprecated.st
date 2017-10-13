tests - testing
testIsDeprecated
	| deprecatedSelectors |
	deprecatedSelectors := #(deprecatedMethod deprecatedMethod2 deprecatedMethod3 deprecatedMethod4 deprecatedMethod5 deprecatedMethod6 deprecatedMethod7)
		asSet.
	self class
		selectorsDo: [ :each | 
			(deprecatedSelectors includes: each)
				ifTrue: [ self assert: (self class >> each) isDeprecated ]
				ifFalse: [ self deny: (self class >> each) isDeprecated ] ]
accessing
defaultResources
	^self tests
		inject: OrderedCollection new
		into:
			[:coll :testCase |
			testCase resources do:
				[:each |
				(coll includes: each) ifFalse: [coll add: each]].
			coll]
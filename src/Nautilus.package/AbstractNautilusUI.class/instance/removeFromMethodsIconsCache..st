icon caches
removeFromMethodsIconsCache: aMethod
	self methodsIconsCache removeKey: aMethod ifAbsent: [  ].

	"If aMethod is a TestCase test method or if it's tested by one, then
	clear the test method result and remove it and all its tested methods
	from the icon cache."
	aMethod asTestMethod
		ifNotNil: [ :testMethod |
			testMethod methodClass clearResultOfTestMethod: testMethod.
			self methodsIconsCache removeKey: testMethod ifAbsent: [  ].
			testMethod correspondingTestedMethods do: [ :each |
				self methodsIconsCache removeKey: each ifAbsent: [  ] ] ].
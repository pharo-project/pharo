icon caches
removeFromMethodsIconsCache: aMethod
	self methodsIconsCache removeKey: aMethod ifAbsent: [  ].

	aMethod correspondingTestMethod
		ifNil: [
			aMethod correspondingTestedMethods
				ifNotEmpty: [ :testedMethods |
					"It's a test method with corresponding tested methods; clear its test
					result and remove the icons of its tested methods from the cache"
					aMethod methodClass clearResultOfTestMethod: aMethod.
					testedMethods do: [ :each |
						self methodsIconsCache removeKey: each ifAbsent: [  ] ]]]
		ifNotNil: [ :testMethod |
			"It's a method with a corresponding test method; clear its test
			method's result (because the method it tests has changed) and remove
			the it from the cache"
			testMethod methodClass clearResultOfTestMethod: testMethod.
			self methodsIconsCache removeKey: testMethod ifAbsent: [  ] ]
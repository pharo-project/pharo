private
runTestForMethods: aCollection notifying: aBoolean

	aCollection
		do: [ :each | 
			each isTestMethod
				ifTrue: [ self runTestForAMethod: each notifying: aBoolean ]]
test - integrity
testAllDictionariesAreHealthy
	self canBeUnhealthy
		ifTrue: [
			self collectionClass allSubInstances do: [ :dict |
				self assert: dict isHealthy ] ]
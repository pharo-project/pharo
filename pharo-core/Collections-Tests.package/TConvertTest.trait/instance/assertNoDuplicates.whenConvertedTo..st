tests - converting
assertNoDuplicates: aCollection whenConvertedTo: aClass 
	| result |
	result := self collectionWithEqualElements asIdentitySet.
	self assert: (result class includesBehavior: IdentitySet).
	self collectionWithEqualElements do: [ :initial | self assert: (result occurrencesOf: initial) = 1 ]
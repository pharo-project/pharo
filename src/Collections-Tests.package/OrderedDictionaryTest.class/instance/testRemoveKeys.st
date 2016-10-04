tests
testRemoveKeys
	0 to: self orderedAssociations size do: [:i | | dictionary keysToRemove |
		dictionary := self dictionaryWithOrderedAssociations.

		"make it a set to ensure it supports non-Sequenceable collections"
		keysToRemove :=
			(self isTestingIdentityDictionary
				ifTrue: [IdentitySet]
				ifFalse: [Set])
				withAll: (self orderedKeysFirst: i).
		self assert: (dictionary removeKeys: keysToRemove) == dictionary.
		keysToRemove do: [:each |
			self
				assertKey: each
				wasRemovedfrom: dictionary].

		self
			assertIsDictionary: dictionary
			withOrderedAssociations: (self orderedAssociationsAllButFirst: i)]
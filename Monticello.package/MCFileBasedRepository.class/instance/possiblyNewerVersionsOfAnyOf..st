accessing
possiblyNewerVersionsOfAnyOf: someVersions
	| pkgs |
	pkgs := Dictionary new.

	someVersions do: [:aVersionInfo |
		pkgs at: (aVersionInfo name copyUpToLast: $-)
			put: (aVersionInfo name copyAfterLast: $.) asNumber].

	^[self allVersionNames select: [:each |
		(pkgs at: (each copyUpToLast: $-) ifPresent: [:verNumber |
			verNumber < (each copyAfterLast: $.) asNumber
				or: [verNumber = (each copyAfterLast: $.) asNumber
					and: [someVersions noneSatisfy: [:v | v name = each]]]]) == true]
	] on: Error do: [:ex | ex return: #()]
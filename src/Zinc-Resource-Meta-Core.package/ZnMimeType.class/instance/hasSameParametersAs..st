testing
hasSameParametersAs: aMimeType
	^ self hasParameters = aMimeType hasParameters
		and: [self parameters size = aMimeType parameters size
			and: [self parameters associations allSatisfy: [:each |
				aMimeType parameters includesAssociation: each]]]
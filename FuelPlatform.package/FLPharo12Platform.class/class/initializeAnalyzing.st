private-extensions-flsubstitutioncluster
initializeAnalyzing
	<flExtensionOf: #FLSubstitutionCluster isSource: true>
	^ 'initializeAnalyzing

		super initializeAnalyzing.
		substitutions := IdentityDictionary new.
		substitutes := OrderedCollection new'
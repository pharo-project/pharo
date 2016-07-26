enquiries
allAliasesDict
	| dict |
	dict := super allAliasesDict.
	self aliases do: [:assoc |
		dict at: assoc key put: assoc value].
	^dict
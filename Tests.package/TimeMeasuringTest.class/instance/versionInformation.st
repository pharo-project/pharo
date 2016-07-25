as yet unclassified
versionInformation
	| wcPredicate |
	wcPredicate := self workingCopyPredicate.
	^self versionInfoForWorkingCopiesThat: wcPredicate
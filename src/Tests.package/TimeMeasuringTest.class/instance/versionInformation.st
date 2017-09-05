accessing
versionInformation
	| wcPredicate |
	wcPredicate := self workingCopyPredicate.
	^self versionInfoForWorkingCopiesThat: wcPredicate
actions
clearRequiredPackages
	workingCopy ifNotNil:
		[:wc |
		wc clearRequiredPackages.
		self workingCopyListChanged]
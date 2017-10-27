extended bytecode generation
genBranchPopTrueNoMustBeBoolean: distance
	self genNoMustBeBooleanFlag.
	self genBranchPopTrueLong: distance
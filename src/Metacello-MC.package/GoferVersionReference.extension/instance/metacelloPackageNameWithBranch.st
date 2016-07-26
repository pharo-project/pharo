*metacello-mc
metacelloPackageNameWithBranch
	"answer array with package name and package name with branch name"
	self branch isEmpty
		ifTrue: [ 
			^ {(self packageName).
			(self packageName)} ].
	^ {(self packageName).
	(self packageName , '.' , self branch)}
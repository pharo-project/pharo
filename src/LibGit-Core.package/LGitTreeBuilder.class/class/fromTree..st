instance creation
fromTree: aTree
	self validate: aTree shouldBe: LGitTree.
	^ self of: aTree repository fromTree: aTree
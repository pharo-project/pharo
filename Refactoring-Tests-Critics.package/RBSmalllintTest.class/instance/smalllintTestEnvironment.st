private
smalllintTestEnvironment
	| classEnvironment |
	classEnvironment := RBClassEnvironment new.
	classEnvironment addClass: RBSmalllintTestObject.
	^ classEnvironment
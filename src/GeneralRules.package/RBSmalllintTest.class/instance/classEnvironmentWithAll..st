private
classEnvironmentWithAll: aClassCollection
	| classEnvironment |
	classEnvironment := RBClassEnvironment new.
	aClassCollection do: [ :each | classEnvironment addClass: each ].
	^ classEnvironment
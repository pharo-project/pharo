groups
buildMethodGroupManager
	| holder |
	holder := GroupHolder new.
	(holder addAMethodGroupSilentlyNamed: self lastModifiedMethodsGroupName methods: self lastModifiedMethods)
		removable: false.
	^ holder
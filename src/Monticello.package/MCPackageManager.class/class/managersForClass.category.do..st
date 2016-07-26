system changes
managersForClass: aClass category: methodCategory do: aBlock
	(methodCategory isEmptyOrNil or:[methodCategory first ~= $*]) ifTrue:[
		"Not an extension method"
		^self managersForClass: aClass do: aBlock.
	].
	self managersForCategory: methodCategory allButFirst do: aBlock.
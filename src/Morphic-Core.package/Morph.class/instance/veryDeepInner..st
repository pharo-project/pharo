copying
veryDeepInner: deepCopier 
	"The inner loop, so it can be overridden when a field should not  
	be traced."
	"super veryDeepInner: deepCopier.	know Object has no inst vars"
	bounds := bounds shallowCopy.
	"Points are shared with original"
	"owner := owner.	special, see veryDeepFixupWith:"
	submorphs := submorphs veryDeepCopyWith: deepCopier.
	"each submorph's fixup will install me as the owner"
	"fullBounds := fullBounds.	fullBounds is shared with original!"
	color := color veryDeepCopyWith: deepCopier.
	"color, if simple, will return self. may be complex"
	extension := (extension veryDeepCopyWith: deepCopier)
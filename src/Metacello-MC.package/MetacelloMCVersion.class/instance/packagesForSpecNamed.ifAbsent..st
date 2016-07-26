querying
packagesForSpecNamed: aString ifAbsent: aBlock
	"resolves list of packages associated with the named spec.
	   If the spec is a packages, answer a list including only the package. #requires: and #includes:
			fields in the package are ignored.
	   If the spec is a project, answers an empty list.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"
	"If there is no spec named <aString>, aBlock is evaluated"

	| pkgSpec |
	pkgSpec := self spec packageNamed: aString ifAbsent: aBlock.
	^pkgSpec resolveToPackagesIn: self spec visited: MetacelloVisitedPackages new
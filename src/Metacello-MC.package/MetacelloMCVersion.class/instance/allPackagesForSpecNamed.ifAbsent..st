querying
allPackagesForSpecNamed: aStringOrArray ifAbsent: aBlock
	"resolves list of packages associated with the named spec.
	   If the spec is a packages, answer a list including the package and the transitive closure on 
			its #requires: and #includes: fields.
	   If the spec is a project, answer a list of the packages associated with the project,
			following the transitive closure on packages reachable starting with the #loads: field.
	   If the spec is a group, answers the list of packages in the #includes: field of the group. 
			Groups in the #includes: field are expanded following the transitive closure on groups"
	"In essence, this query answers the list of all packages that would be loaded if the package 
		named <aString> were loaded."
	"If there is no spec named <aString>, aBlock is evaluated"

	^aStringOrArray 
		resolvePackageSpecsNamedForMetacelloMCVersion: self 
		visited: MetacelloVisitedPackages new 
		ifAbsent: aBlock
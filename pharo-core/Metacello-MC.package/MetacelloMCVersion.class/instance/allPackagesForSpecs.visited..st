private
allPackagesForSpecs: pkgSpecs visited: visited

 
	| coll |
	coll := Dictionary new.
	pkgSpecs
		do: [:pkgSpec | (pkgSpec resolveToAllPackagesIn: self spec visited: visited)
				do: [:pkg | coll at: pkg name put: pkg ]].
	^ coll values asOrderedCollection
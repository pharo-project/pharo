*metacello-mc
resolveToAllPackagesIn: aVersionSpec into: packages visited: visited

	visited
		visit: self
		doing: [:spec |
			self visitingWithPackages: packages.
			(spec includes, spec requires) do: [:pkgName |
				(aVersionSpec packageNamed: pkgName)
					projectDo: [:prj | 
						(prj resolveToAllPackagesIn: aVersionSpec visited: visited) do: [:pkg | 
							packages at: pkg name put: pkg ]] 
					packageDo: [:pkg | 
						packages at: pkg name put: pkg.
						(pkg resolveToAllPackagesIn: aVersionSpec visited: visited) do: [:rpkg | 
							packages at: rpkg name put: rpkg ] ] 
					groupDo: [:grp | grp resolveToAllPackagesIn: aVersionSpec into: packages  visited: visited ]]]
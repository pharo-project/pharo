*metacello-mc
resolveToAllPackagesIn: aVersionSpec into: packages visited: visited

	(self resolveProjectSpec resolveToAllPackagesIn: aVersionSpec visited: visited) do: [:pkg |
		packages at: pkg put: pkg ]
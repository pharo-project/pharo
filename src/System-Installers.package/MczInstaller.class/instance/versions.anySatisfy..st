utilities
versions: aVersionList anySatisfy: aDependencyID
	^ aVersionList anySatisfy: [:version | 
			aDependencyID = (version at: #id)
				or: [self versions: (version at: #ancestors) anySatisfy: aDependencyID]]
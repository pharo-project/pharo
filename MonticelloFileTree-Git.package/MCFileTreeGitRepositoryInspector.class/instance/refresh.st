accessing
refresh
	loadedPackages := Set new.
	versions := repository retrieveVersionsWithPackageNames: Set new.
	newer := Set new.
	loaded := Set new.
	MCWorkingCopy allManagers
		do: [ :eachManager | 
			| latestVersions |
			loadedPackages add: eachManager packageName.
			eachManager ancestors do: [ :each | loaded add: each name ].
			latestVersions := versions select: [ :each | each first beginsWith: eachManager packageName ].
			latestVersions
				do: [ :eachVersion | 
					| maxVersionWithAncestors |
					maxVersionWithAncestors := eachManager ancestors max: #versionNumber.
					eachManager ancestors
						do: [ :eachAncestor | 
							eachVersion fourth info = eachAncestor
								ifTrue: [ eachAncestor loadAncestorsAndStepChildrenFrom: eachVersion fourth info ].
							(maxVersionWithAncestors < eachVersion third
								or: [ eachAncestor versionNumber = eachVersion third and: [ eachAncestor author ~= eachVersion second ] ])
								ifTrue: [ newer add: eachVersion last ] ] ] ].
	self packageListUpdate.
	repository branch: nil.
	self changed: #gitNeedPush
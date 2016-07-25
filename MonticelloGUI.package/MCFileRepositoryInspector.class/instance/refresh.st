morphic ui
refresh
	loadedPackages := Set new.
	versions := repository retrieveVersionsWithPackageNames: Set new.
	newer := Set new.
	loaded := Set new.
	MCWorkingCopy allManagers
		do: [ :workingCopy | | versionsForPackage |
			workingCopy ancestors do: [ :ancestor | loaded add: ancestor name ].
			versionsForPackage := versions select: [ :v | v first = workingCopy package name ].
			versionsForPackage
				do: [ :aVersionArray | 
					workingCopy ancestors
						do: [ :ancestor |
							(ancestor versionNumber < aVersionArray third
								or: [ ancestor versionNumber = aVersionArray third and: [ ancestor author ~= aVersionArray second ] ])
								ifTrue: [ newer add: ancestor name ] ] ] ].
	self
		packageListUpdate;
		changed: #versionList
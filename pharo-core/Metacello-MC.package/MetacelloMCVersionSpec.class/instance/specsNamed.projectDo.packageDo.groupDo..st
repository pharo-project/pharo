enumerating
specsNamed: packageAndProjectNames projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock

	| map |
	map := self packages map.
	packageAndProjectNames do: [:name | | pkgSpec |
		(pkgSpec := map at: name ifAbsent: [ ]) ~~ nil
			ifTrue: [ pkgSpec projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock ]]
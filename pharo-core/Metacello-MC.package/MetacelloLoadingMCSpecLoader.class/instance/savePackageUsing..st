development support
savePackageUsing: repositorySpecs

	| wc repo repoSpecs newVersion |
	(wc := self spec workingCopy) == nil ifTrue: [ ^false ].
	(wc ancestry ancestors notEmpty and: [ wc modified not])
		ifTrue: [ ^false ].
	repoSpecs := self spec repositorySpecs notEmpty
				ifTrue: [ self spec repositorySpecs ]
				ifFalse: [ repositorySpecs ].
	(self spec getFile == nil or: [ wc ancestry ancestors isEmpty ])
		ifTrue: [ repo := (self repositoriesFrom: repoSpecs ignoreOverrides: true) first ]
		ifFalse: [
			[ 	| newer |
				repo := (self resolveSpec: self spec from: repoSpecs) repository.
				newer := wc possiblyNewerVersionsIn: repo.
				newer isEmpty not 
					ifTrue: [
						(MetacelloPlatform current confirm: 'CAUTION! These versions in the repository may be newer:', 
							String cr, newer printString, String cr,
							'Do you really want to save this version?') ifFalse: [ ^false ]] ] 
				on: Error 
				do: [:ignored | ^false ] ].
	(newVersion :=  MetacelloPlatform current newVersionForWorkingCopy: wc) == nil ifTrue: [ ^false ].
	repo storeVersion: newVersion.
	^true
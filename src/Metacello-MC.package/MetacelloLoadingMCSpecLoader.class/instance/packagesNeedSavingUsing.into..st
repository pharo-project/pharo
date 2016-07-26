development support
packagesNeedSavingUsing: repositorySpecs into: aCollection

	| wc repoSpecs repo |
	(wc := self spec workingCopy) == nil ifTrue: [ ^self ].
	(wc ancestry ancestors notEmpty and: [ wc modified not])
		ifTrue: [ ^self ].
	repoSpecs := self spec repositorySpecs notEmpty
				ifTrue: [ self spec repositorySpecs ]
				ifFalse: [ repositorySpecs ].
	repo := (self spec getFile == nil or: [ wc ancestry ancestors isEmpty ])
		ifTrue: [ (self repositoriesFrom: repoSpecs ignoreOverrides: true) first ]
		ifFalse: [
			([ self resolveSpec: self spec from: repoSpecs ] 
				on: Error 
				do: [:ignored | ^self ]) repository ].
	aCollection add: self spec -> repo
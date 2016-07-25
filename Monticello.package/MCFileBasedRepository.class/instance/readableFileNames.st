accessing
readableFileNames
	| all cached new emptyFilenamelength |
	"<hyphenated-package-name>.<dotted.branch.tag>-<initials>.<count>.mcz"
	emptyFilenamelength := 'P-i.c.mcz' size.
	all := self allFileNames.	"from repository"
		
	all := all reject: [ :each |  each size < emptyFilenamelength].
		"first stupid way to filter first level broken files. Ideally we should 
		remove any files not following the naming pattern: PackageName-author.number[(branch)].mcz"
	
	"Another stupid way to remove filename when using filetree repositories."
	all := all reject: [ :each | each beginsWith: '.filetree' ].
	
	cached := self cachedFileNames.	"in memory"
	new := all difference: cached.
	^ (cached asArray, new)
		select: [:ea | self canReadFileNamed: ea]
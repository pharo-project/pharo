doits
changeRepositoryType
	| repos newRepo |
	(Smalltalk hasClassNamed: #MCGitRepository) ifFalse: [ ^ self ].
	
	repos := MCRepositoryGroup default repositories
		select: [ :r | (r description includesSubstring: 'LibGit:master') or: [ 
			r description includesSubstring: 'LibGit/master' ] ].
	repos ifEmpty: [  ^ self ].
	
	'github-cache/theseion' asFileReference deleteAll.
	newRepo := (Smalltalk at: #MCGitRepository) new
		repoUrl: 'git@github.com:theseion/LibGit';
		ensureDirectory;
		yourself.
		
	MCRepositoryGroup allInstances
		select: [ :group | group repositories includesAnyOf: repos ]
		thenDo: [ :group | 
			repos do: [ :repo | group removeRepository: repo ].
			group addRepository: newRepo ]
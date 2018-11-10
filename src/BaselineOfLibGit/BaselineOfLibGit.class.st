Class {
	#name : #BaselineOfLibGit,
	#superclass : #BaselineOf,
	#category : #BaselineOfLibGit
}

{ #category : #baseline }
BaselineOfLibGit >> baseline: spec [
  <baseline>
  spec
    for: #(common)
    do: [ 
      spec
    	  package: 'LibGit-FileSystem' with: [ spec requires: 'LibGit-Core' ];    
		  package: 'LibGit-Tests' with: [ spec requires: 'LibGit-Core' ];
        package: 'LibGit-GT' with: [ spec requires: 'LibGit-Core' ];
        package: 'LibGit-Patches' with: [ spec requires: 'LibGit-Core' ];
        package: 'LibGit-Core'.
      spec
        group: 'default'
        with: #('LibGit-Core' 'LibGit-FileSystem').
		spec
			group: 'development'
			with: #('default' 'LibGit-Tests' 'LibGit-Patches' 'LibGit-GT').
		spec
			group: 'tests'
			with: #('LibGit-Tests').
		spec postLoadDoIt: #postLoadActions ]

]

{ #category : #doits }
BaselineOfLibGit >> changeRepositoryType [
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
]

{ #category : #doits }
BaselineOfLibGit >> postLoadActions [
	self changeRepositoryType
]

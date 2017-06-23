initialization
computeReferenceCommit
	| candidates |
	
	"1. If no code has been loaded, take HEAD commit as reference."
	self loadedVersions isEmpty ifTrue: [ ^ self repository headCommit ].
		
	"2. Happy path: see if all loaded code has the same updation commit. 
	If we can't compute an updation commit for some loaded code, it means that it has been 
	probably loaded from another kind of repository, we just ignore it."
	candidates := (self loadedVersions collect: [:version | version commit] as: Set) reject: #isNil.
	candidates size = 1 ifTrue: [ ^ candidates anyOne ].

	"3. None of the loaded versions produced a non nil candidate, just use the HEAD commit"	
	candidates isEmpty ifTrue: [ ^ self repository headCommit ].

	"4. We could try other strategies... but this should be good for now."	
	^ candidates detectMax: #datetime
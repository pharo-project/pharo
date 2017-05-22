operations
merge: anotherCommit
	"Merge two commits, producing a git_index that reflects the result of the merge."
	| index | 
	index := LGitIndex of: self repository.
	self withReturnHandlerDo: [ 
		self merge_result: index commit: anotherCommit options: LGitMergeOptions defaults.
	].

	"I think that there is a bug in version 0.23 of libgit2, the returned index does not have an owner (a pointer to a repository), 
	as you would expect from the documentation. This breaks libgit encapsulation, and will probably fail if updating to a newer version,
	but anyway if we migrate this bug should be fixed."
	index handle unsignedLongAt: 5 put: repository handle asInteger.
	^ index

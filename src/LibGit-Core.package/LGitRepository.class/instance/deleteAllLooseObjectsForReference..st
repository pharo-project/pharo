private
deleteAllLooseObjectsForReference: aReference
	(self repositoryReference / '.git' / 'objects') children
		select: [ :entry | entry basename ~= 'pack' ]
		thenDo: [ :entry | entry deleteAll ].
	
	"raise an exception if not resolvable"
	self head
	
"	| walker oid baseReference |
	walker := LGitRevwalkType new.
	LG2RevwalkH uniqueInstance
		revwalk_new: walker
		repo: self.
	LG2RevwalkH uniqueInstance
		revwalk_sorting: walker
		sort_mode: 2.
	LG2RevwalkH uniqueInstance
		revwalk_push_ref: walker
		refname: aReference name.

	baseReference := aReference repository repositoryReference / '.git' / 'objects'.
	oid := LGitId new.
	[ (LG2RevwalkH uniqueInstance
		revwalk_next: oid
		walk: walker) isZero ] whileTrue: [
			| fileReference |
			fileReference := baseReference resolve: oid asRelativePathString.
			fileReference delete.
			fileReference parent hasChildren ifFalse: [ fileReference parent delete ] ]"
traversing
subTreeEntries
	^ self entries select: [ :entry |
		entry type = LGitObjectTypeEnum git_obj_tree ]
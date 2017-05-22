testing
isLeaf
	^ self entries noneSatisfy: [ :entry |
		entry type = LGitObjectTypeEnum git_obj_tree  ]
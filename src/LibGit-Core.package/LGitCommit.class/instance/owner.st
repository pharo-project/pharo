accessing
owner
	^ self 
		call: #(void * git_commit_owner(self)) 
		options: #()

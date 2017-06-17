accessing
owner
	^ self call: #(void * git_reference_owner #(self)) options: #()
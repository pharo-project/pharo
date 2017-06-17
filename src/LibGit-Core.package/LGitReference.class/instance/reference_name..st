libgit-calls
reference_name: ref
	
	^ self call: #(String git_reference_name #(self)) options: #()
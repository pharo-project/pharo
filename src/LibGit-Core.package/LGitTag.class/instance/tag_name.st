libgit-calls
tag_name
	^ self call: #(String git_tag_name #(self)) options: #()
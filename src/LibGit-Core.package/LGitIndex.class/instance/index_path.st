libgit-calls
index_path
	^ self
		call: #(String git_index_path #(self))
		options: #()
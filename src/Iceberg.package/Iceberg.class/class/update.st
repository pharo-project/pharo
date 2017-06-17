updating
update
	#(Iceberg LGitRepository MCFileTreeGitRepository) do: [:className | 
		Smalltalk 
			at: className 
			ifPresent: [ :class | class package iceRepository pull ] ]
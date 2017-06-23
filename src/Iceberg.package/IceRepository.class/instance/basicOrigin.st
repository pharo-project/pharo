private
basicOrigin
	^ [ self backend origin ]
		on: LGit_GIT_ENOTFOUND 
		do: [ :e | nil ]
actions
init
	"Initialize new repository (git init)"
	LGitGlobal runSequence: [ 
		(LGitRepository on: self location) init ]
repository handling
command: aCommandDescription
	"See IceGitCommand for the specification of possible command descriptions"
	self ensureLocalRepository.
	^ IceGitCommand in: self location execute: aCommandDescription

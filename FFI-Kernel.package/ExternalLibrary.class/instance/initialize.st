initialize-release
initialize
	"Initialize the receiver"
	name := self class moduleName.
	handle := ExternalAddress new.
system simulation
stepToCallee
	"Step to callee or sender"

	| context |
	context := self.
	[ (context := context step) == self ] whileTrue.
	^ context
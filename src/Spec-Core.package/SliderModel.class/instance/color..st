api
color: aColor

	"Hack because during the interpretation, the state is slightly inconistent"
	self widget ifNotNil: [:w | w == self ifFalse: [ super color: aColor ]]
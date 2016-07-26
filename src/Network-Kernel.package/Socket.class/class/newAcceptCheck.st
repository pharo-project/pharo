tests
newAcceptCheck
	"Check if the platform has support for the BSD style accept()."

	"Socket newAcceptCheck"
	
	| socket |
	self initializeNetwork.
	socket := self newTCP.
	socket listenOn: 44444 backlogSize: 4.
	socket isValid ifTrue: [
		self inform: 'Everything looks OK for the BSD style accept()'
	] ifFalse: [
		self inform: 'It appears that you DO NOT have support for the BSD style accept()'].
	socket destroy
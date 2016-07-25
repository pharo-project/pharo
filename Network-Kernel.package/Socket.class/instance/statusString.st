queries
statusString
	"Return a string describing the status of this socket."

	| status |
	socketHandle == nil ifTrue: [^ 'destroyed'].
	status := self primSocketConnectionStatus: socketHandle.
	status = InvalidSocket ifTrue: [^ 'invalidSocketHandle'].
	status = Unconnected ifTrue: [^ 'unconnected'].
	status = WaitingForConnection ifTrue: [^ 'waitingForConnection'].
	status = Connected ifTrue: [^ 'connected'].
	status = OtherEndClosed ifTrue: [^ 'otherEndClosedButNotThisEnd'].
	status = ThisEndClosed ifTrue: [^ 'thisEndClosedButNotOtherEnd'].
	^ 'unknown socket status'

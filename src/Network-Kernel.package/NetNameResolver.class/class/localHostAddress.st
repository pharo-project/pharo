lookups
localHostAddress
	"Return the local address of this host."
	"NetNameResolver localHostAddress"
	<todo>
	"On Mac the primitive is buggy and can return an empty IP address. Use a standard value in that case"
		
	| primAddress |

	self initializeNetwork.
	primAddress := self primLocalAddress.
	^ primAddress = #[0 0 0 0] ifTrue: [ #[127 0 0 1] ] ifFalse: [ primAddress ]
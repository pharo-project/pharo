scripting
to: aPort
	"Ideally this method would be implemented as
	^ self to: aPaneSymbol port: #entiity.
	
	The problem is that beforehand we only had from: which could receive either a symbol for the pane, or an association for the port. This was bad, and now Glamour offers to:port:, but there are still legacy cases of to: being called with an association."

	^ self destination: (self browser resolveDestinationPort: aPort)
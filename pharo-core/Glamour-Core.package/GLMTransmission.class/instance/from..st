scripting
from: aPort
	"Ideally this method would be implemented as
	^ self from: aPaneSymbol port: #selection.
	
	The problem is that beforehand we only had from: which could receive either a symbol for the pane, or an association for the port. This was bad, and now Glamour offers from:port:, but there are still legacy cases of from: being called with an association."

	^ self addActiveOrigin: (self browser resolveOriginPort: aPort)
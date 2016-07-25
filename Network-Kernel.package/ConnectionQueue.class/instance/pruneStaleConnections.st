private
pruneStaleConnections
	"Private! The client may establish a connection and then disconnect while it is still in the connection queue. This method is called periodically to prune such sockets out of the connection queue and make room for fresh connections."

	accessSema critical: [
		| foundStaleConnection |
		foundStaleConnection := false.
		connections do: [:s |
			s isUnconnected ifTrue: [
				s destroy.
				foundStaleConnection := true]].
		foundStaleConnection ifTrue: [
			connections := connections select: [:s | s isValid]]].

registry
unregister: anObject
	"An attachable file stream is generally either a second reference to an
	existing file stream, or a reference to a transient object such as a pipe
	endpoint. There is no need to register it for finalization."

	^ anObject
accessing
bundle
	"It returns all collected events that are supposed to be sent. If you need to send just a portion of the collected events, this is the place to overwrite."
	^ self bundle: OrderedCollection new.
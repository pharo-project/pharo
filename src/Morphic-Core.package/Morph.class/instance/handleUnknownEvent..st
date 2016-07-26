events-processing
handleUnknownEvent: anEvent
	"An event of an unknown type was sent to the receiver. What shall we do?!"
	self inform: 'Unknown event: ', anEvent printString. 
	anEvent printString displayAt: 0@0.
	anEvent wasHandled: true.
private
startUp
	"This functionality is disabled for now, to avoid doing lots of processing
	on start up."
	
	"We're starting up in a new OS process, so the file id will be invalid.
	Try to reopen the file, but fail silently: just leave the id as nil. #isOpen will
	answer false, and we'll raise an error if anyone tries to do IO."
	
	self basicOpen
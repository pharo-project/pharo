testing
isTerminating
	"lazy initialization is a fallback only for processes that existed before this addition"
	^ terminating ifNil: [ false ]
protocol
replaceCurrentWith: anEntry
	"Change the current history item to the given object"
	storage at: index put: (anEntry -> DateAndTime now)
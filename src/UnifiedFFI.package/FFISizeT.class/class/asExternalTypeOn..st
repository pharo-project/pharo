converting
asExternalTypeOn: generator
	"We resolve size_t to a uint/ulong which may be not the case always"
	^ generator resolveType: #ulong
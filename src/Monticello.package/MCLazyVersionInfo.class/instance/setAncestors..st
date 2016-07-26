private
setAncestors: someAncestors
	"Unused in MCVersionInfo"

	"It is there to allow a certain customisation with lazy stuff without kernel dependencies on LRUCache"

	ancestors := WeakArray withAll: someAncestors
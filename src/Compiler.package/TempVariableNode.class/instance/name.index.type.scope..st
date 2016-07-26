initialize-release
name: varName index: i type: type scope: level
	"Only used for initting temporary variables"
	hasDefs := hasRefs := false.
	scope := level.
	^super name: varName key: varName index: i type: type
accessing
default
	"InputEventFetcher default"

	^Default ifNil: [Default := InputEventFetcher new]
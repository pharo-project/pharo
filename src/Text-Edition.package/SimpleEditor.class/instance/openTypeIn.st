typing - support
openTypeIn
	"Set up beginTypeInIndex to keep track of the leftmost backspace."

	beginTypeInIndex ifNil: [
		beginTypeInIndex := self startIndex ]
private - copying
readOnlyDo: aBlock
	"Value a block with a read only copy of self."

	| copy |
	copy := self readOnlyCopy.

	^ [ aBlock value: copy ] ensure: [ copy close ]
fields accessing
indexableSizeOf: anObject
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size"

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0
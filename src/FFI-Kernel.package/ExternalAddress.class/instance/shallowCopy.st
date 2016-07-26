copying
shallowCopy
	"Re-implemented to avoid superclass call to #new:"
	"But superclass's shallowCopy sends basicNew: and basicNew: is ok. eem 2/21/2016 15:31"
	^self clone
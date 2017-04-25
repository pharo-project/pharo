converting
asTruncatedTextFrom: anObject
	"I return a truncated representation of the receiver in which all lines breaks 
	are replaced by spaces. I return a String unless there is an error printing the
	object. In this case I return a Text highlighted in red."
	^ [ (anObject gtDisplayString copyReplaceAll: String cr with: String space) replaceAll: String lf with: String space ]
		on: Error
		do: [ Text string: 'error printing' attribute: TextColor red ]
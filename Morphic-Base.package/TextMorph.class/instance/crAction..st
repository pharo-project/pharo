accessing
crAction: aMessageSend
	"Return the action to perform when encountering a CR in the input"
	^self setProperty: #crAction toValue: aMessageSend
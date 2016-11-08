printing
asTruncatedTextFrom: anObject
	^ [ Text fromString: ((anObject gtDisplayString copyReplaceAll: String cr with: String space) copyReplaceAll: String lf with: String space) ]
		on: Error
		do: [ Text string: 'error printing' attribute: TextColor red ]
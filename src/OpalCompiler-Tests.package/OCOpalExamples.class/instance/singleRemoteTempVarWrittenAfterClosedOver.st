examples
singleRemoteTempVarWrittenAfterClosedOver
	| index block |
	index := 0.
	block := [ index ].
	index := index + 1.

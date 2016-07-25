evaluation
valueWithContext: vmCallbackContext "<VMCallbackContext32>" "^<Integer>"
	<legacy>
	^block
		value: (argsProxyClass
					atAddress: vmCallbackContext rawStackp
					dataSize: argsProxyClass dataSize)
		value: vmCallbackContext
helpers
newCommand
	| command |
	command := self commandClass new.
	command defaultReadStreamCreationBlock: [ OSSUnixSubprocess createTempFileToBeUsedAsReadStreamOn: '/tmp' ].
	command defaultWriteStreamCreationBlock: [ OSSUnixSubprocess createTempFileToBeUsedAsWriteStreamOn: '/tmp' ].
	^ command 
	
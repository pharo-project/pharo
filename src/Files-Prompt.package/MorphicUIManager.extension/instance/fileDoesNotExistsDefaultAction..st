*Files-Prompt
fileDoesNotExistsDefaultAction: anException

	^ anException readOnly
		ifTrue: [ self readOnlyFileDoesNotExistUserHandling: anException fileName]
		ifFalse: [self fileDoesNotExistUserHandling: anException fileName]

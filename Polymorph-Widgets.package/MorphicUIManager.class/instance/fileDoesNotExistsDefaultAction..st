ui requests
fileDoesNotExistsDefaultAction: anException

	^anException readOnly
		ifTrue: [StandardFileStream readOnlyFileDoesNotExistUserHandling: anException fileName]
		ifFalse: [StandardFileStream fileDoesNotExistUserHandling: anException fileName]

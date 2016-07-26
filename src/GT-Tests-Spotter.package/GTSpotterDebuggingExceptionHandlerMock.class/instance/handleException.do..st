public
handleException: anException do: aBlock
	" we don't want to perform aBlock during tests --> suppress logging to transcript "
	" we perform the #pass after we register that the exception happened - otherwise we cannot test for it later "
	GTSpotterExceptionsTest exceptions add: anException.
	anException pass
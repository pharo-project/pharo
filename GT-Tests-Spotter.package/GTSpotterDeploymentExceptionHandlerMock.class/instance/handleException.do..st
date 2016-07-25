public
handleException: anException do: aBlock
	" we don't want to perform aBlock during tests --> suppress logging to transcript "
	" only register the occured exception for later testing "
	GTSpotterExceptionsTest exceptions add: anException.
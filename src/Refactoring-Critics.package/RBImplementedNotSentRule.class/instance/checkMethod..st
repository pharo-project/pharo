running
checkMethod: aMethod
	"Check if there are any senders. Furthermore methods with pragmas are likely to be sent through reflection, thus do not report those. Also test methods are sent through reflection, so ignore those as well."
	(self basicCheck: aMethod) ifTrue: [
		result addMethod: aMethod ]
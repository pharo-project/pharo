running
checkMethod: aMethod 
	(self basicCheck: aMethod) ifTrue: 
		[ result
			addMethod: aMethod;
			searchStrings: #('True' 'False' ) ]
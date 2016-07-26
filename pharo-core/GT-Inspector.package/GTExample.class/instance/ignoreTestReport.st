testing
ignoreTestReport
	^ self propertyAt: #noTestReport ifAbsent: [ false ]
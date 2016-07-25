testing
isValidPropertyNameType: associationKey
	^ allowComplexMapKeys or: [associationKey isString or: [ associationKey isNumber ] ]
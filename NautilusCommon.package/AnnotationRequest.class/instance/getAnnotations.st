actions
getAnnotations
	^ String streamContents: [ :aStream | 
		((self annotationRequests 
			collect: [ :request | self perform: request ]) 
			reject: [ :stringOrNil | stringOrNil isEmptyOrNil ]) 
				do: [ :each | aStream nextPutAll: each ]
				separatedBy: [ aStream nextPutAll: separator ] ]
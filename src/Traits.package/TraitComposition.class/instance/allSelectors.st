enquiries
allSelectors
	^ Array streamContents: [ :stream | 
		self transformations do: [ :transformation | stream nextPutAll: transformation allSelectors ]]
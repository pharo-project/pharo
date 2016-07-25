private
postOpen

	reference ifNotNil: [
		self changed: #(openPath) , reference pathSegments. 
	].

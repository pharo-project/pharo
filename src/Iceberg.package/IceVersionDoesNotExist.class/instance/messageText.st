accessing
messageText
	^ 'There is no version for package: {1} in  commit {2}' 
		format: { 
			self version packageName.
			self version commit shortId
		}
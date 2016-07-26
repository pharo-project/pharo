accessing
credentialsForUrl: urlObject
	^ self credentials values select: [ :cred | urlObject authority includesSubstring: (cred domain) ]
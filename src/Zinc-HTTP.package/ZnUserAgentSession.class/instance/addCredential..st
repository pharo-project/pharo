accessing
addCredential: aCredential
	self credentials at: aCredential domain, '@', aCredential realm put: aCredential 
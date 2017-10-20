instance creation
newFrom: aString named: aFileName
	^(self new) contents: aString; localFileName: aFileName; yourself
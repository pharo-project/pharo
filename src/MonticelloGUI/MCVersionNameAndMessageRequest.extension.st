Extension { #name : #MCVersionNameAndMessageRequest }

{ #category : #'*MonticelloGUI' }
MCVersionNameAndMessageRequest >> defaultAction [
	^ MCSaveVersionDialog new
		versionName: suggestion;
		logMessage: suggestedLogComment;
		showModally
]

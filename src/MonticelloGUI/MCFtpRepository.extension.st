Extension { #name : #MCFtpRepository }

{ #category : #'*MonticelloGUI' }
MCFtpRepository class >> fillInTheBlankRequest [
	^ 'FTP Repository:'

	
]

{ #category : #'*MonticelloGUI' }
MCFtpRepository class >> morphicConfigure [
	^ self fillInTheBlankConfigure
]

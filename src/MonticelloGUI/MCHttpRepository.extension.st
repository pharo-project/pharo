Extension { #name : #MCHttpRepository }

{ #category : #'*MonticelloGUI' }
MCHttpRepository class >> fillInTheBlankRequest [
	^ 'HTTP Repository:'
			
]

{ #category : #'*MonticelloGUI' }
MCHttpRepository class >> morphicConfigure [
	^ self fillInTheBlankConfigure
]

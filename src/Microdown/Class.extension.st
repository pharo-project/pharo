Extension { #name : #Class }

{ #category : #'*Microdown' }
Class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	aBuilder 
		header: [ aBuilder text: 'Class: '.
					aBuilder text: self name ] withLevel: 1;
		horizontalLine;
		text: aString
]

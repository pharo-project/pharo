Extension { #name : #Class }

{ #category : #'*Microdown' }
Class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	aBuilder 
		header: [:b | 
					b text: 'Class: '.
					b text: self name] withLevel: 1;
		horizontalLine;
		text: aString
]

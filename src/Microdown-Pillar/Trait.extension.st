Extension { #name : #Trait }

{ #category : #'*Microdown-Pillar' }
Trait >> buildMicroDownUsing: aBuilder withComment: aString [
	
	aBuilder 
		header: [:b | 
					b text: 'Trait: '.
					b text: self name] withLevel: 1;
					horizontalLine;
		text: aString
]

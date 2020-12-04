Extension { #name : #MicCommentBlock }

{ #category : #'*Microdown-Pillar' }
MicCommentBlock >> asPillar [ 
		
	^ PRCommentedLine new
			text: text ;
			yourself
			
]

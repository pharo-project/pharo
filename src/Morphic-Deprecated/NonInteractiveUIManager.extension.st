Extension { #name : 'NonInteractiveUIManager' }

{ #category : '*Morphic-Deprecated' }
NonInteractiveUIManager >> requestPassword: queryString [
	^self nonInteractiveWarning: 'Requesting Password'
]

Extension { #name : #Duration }

{ #category : #'*GT-Playground' }
Duration >> gtPlaygroundHumanReadableString [
	self < 1 minute ifTrue: [ ^ 'just now' ].
	self < 2 minutes ifTrue: [ ^ '1 minute ago' ].
	self < 1 hour ifTrue: [ ^ self minutes asString , ' minutes ago' ].
	self < 1 day ifTrue: [ ^ self hours asString , ' hours ago' ].
	self < 1 year ifTrue: [ ^ self days asString , ' days ago' ].
	^ self printString
]

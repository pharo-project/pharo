Extension { #name : #RGComment }

{ #category : #'*Calypso-Ring' }
RGComment >> asText [

	^ (self content ifNil: ['']) asText
]

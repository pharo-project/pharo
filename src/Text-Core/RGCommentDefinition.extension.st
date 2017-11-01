Extension { #name : #RGCommentDefinition }

{ #category : #'*Text-Core' }
RGCommentDefinition >> asStringOrText [
	| text |
	self realClass ifNil: [ ^ self fullName ].
	text := self fullName asText.
	text addAttribute: TextEmphasis italic.
	^ text
]

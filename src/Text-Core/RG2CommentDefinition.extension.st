Extension { #name : #RG2CommentDefinition }

{ #category : #'*Text-Core' }
RG2CommentDefinition >> asStringOrText [
	| text |
	self realClass ifNil: [ ^ self fullName ].
	text := self fullName asText.
	text addAttribute: TextEmphasis italic.
	^ text
]

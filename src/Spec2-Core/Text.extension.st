Extension { #name : #Text }

{ #category : #'*Spec2-Core' }
Text >> localizedForPresenter: aPresenter [

	^ self asString
]

{ #category : #'*Spec2-Core' }
Text >> withAccentuatedCharacter: aCharacter [

	| text index | 
	text := self copy.
	aCharacter ifNil: [ ^ text ].
	index := self asString asLowercase indexOf: aCharacter asLowercase.
	index isZero
		ifTrue: [ ^ text ].

	^ text
		addAttribute: TextEmphasis underlined from: index to: index;
		yourself
]

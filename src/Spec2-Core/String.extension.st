Extension { #name : #String }

{ #category : #'*Spec2-Core' }
String >> asPresenter [ 

	^ SpLabelPresenter new 
		label: self;
		yourself
]

{ #category : #'*Spec2-Core' }
String >> localizedForPresenter: aPresenter [

	"This message serves for the string modification for the purpose of a given Spec presenter. It may, for example, convert it to a presenter's locale. The implementation here (in the String) serves as fallback for regular strings."

	^ self
]

{ #category : #'*Spec2-Core' }
String >> withAccentuatedCharacter: aCharacter [

	| text index | 
	text := self asText.
	aCharacter ifNil: [ ^ text ].
	index := self asLowercase indexOf: aCharacter asLowercase.
	index isZero
		ifTrue: [ ^ text ].

	^ text
		addAttribute: TextEmphasis underlined from: index to: index;
		yourself
]

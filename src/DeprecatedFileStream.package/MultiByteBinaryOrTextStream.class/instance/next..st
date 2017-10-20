accessing
next: anInteger 

	| multiString |
	self isBinary ifTrue: [^ (super next: anInteger) asByteArray].
	multiString := WideString new: anInteger.
	1 to: anInteger do: [:index |
		| character |
		(character := self next) ifNotNil: [
			multiString at: index put: character
		] ifNil: [
			multiString := multiString copyFrom: 1 to: index - 1.
			^ multiString
		]
	].
	^ multiString.

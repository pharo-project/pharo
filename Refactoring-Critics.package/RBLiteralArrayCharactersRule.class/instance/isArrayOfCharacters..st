private
isArrayOfCharacters: anArray 
	anArray isEmpty ifTrue: [^false].
	1 to: anArray size
		do: [:each | (anArray at: each) class == Character ifFalse: [^false]].
	^true
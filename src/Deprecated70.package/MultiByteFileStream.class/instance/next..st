accessing
next: anInteger 

	| multiString |
	self isBinary ifTrue: [^ super next: anInteger].
	multiString := String new: anInteger.
	1 to: anInteger do: [:index |
		| character |
		(character := self next)
			ifNotNil: [ multiString at: index put: character ]
			ifNil: [
				multiString := multiString copyFrom: 1 to: index - 1.
				(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
					ifFalse: [ ^multiString ].
				^self next: anInteger innerFor: multiString ] ].
	(wantsLineEndConversion == true and: [ lineEndConvention notNil ]) "#doConversion is inlined here"
		 ifFalse: [ ^multiString ].

	multiString := self next: anInteger innerFor: multiString.
	(multiString size = anInteger or: [self atEnd]) ifTrue: [ ^ multiString].
	^ multiString, (self next: anInteger - multiString size).

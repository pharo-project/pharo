private-encoding
isStructuredField: aString

	| fName |
	fName := aString copyUpTo: $:.
	('Resent' sameAs: (fName copyUpTo: $-))
		ifTrue: [fName := fName copyFrom: 8 to: fName size].
	^#('Sender' 'From' 'Reply-To' 'To' 'cc' 'bcc') anySatisfy: [:each | fName sameAs: each]
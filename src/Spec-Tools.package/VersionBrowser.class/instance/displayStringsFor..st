accessing
displayStringsFor: aMethodVersion
	| author version cleanStampParts |
	
	"a stamp is a string in the form  'GuillermoPolito 3/4/2015 14:33'"
	"It may be incomplete, empty or even start with spaces."
	cleanStampParts := aMethodVersion stamp trim substrings.
	
	author := cleanStampParts ifEmpty: [ '<missing>' ] ifNotEmpty: [ :parts | parts first ].
	version := cleanStampParts ifEmpty: [ '<missing>' ] ifNotEmpty: [ :parts | parts allButFirst joinUsing: ' ' ].
	
	^ {author.
	version.
	(aMethodVersion realClass name).
	(aMethodVersion selector).
	('{' , aMethodVersion category , '}')}
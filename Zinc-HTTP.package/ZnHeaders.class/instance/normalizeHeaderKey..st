private
normalizeHeaderKey: string
	"Test string to see if it has proper header key capitalization,
	if true, return string, if not, return a properly capitalized copy"
	
	"Optimization: if string is a know,, properly capitalized header, use it as is"
	(CommonHeaders includes: string) 
		ifTrue: [ ^ string ].
	^ (ZnUtils isCapitalizedString: string)
		ifTrue: [ string ] 
		ifFalse: [ ZnUtils capitalizeString: string ] 
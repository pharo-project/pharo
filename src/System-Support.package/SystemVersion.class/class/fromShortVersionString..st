deprecated
fromShortVersionString: aString
	"Create a new SystemVersion from the given short version string. Useful for testing."

	| parts |	
	self 
		deprecated: 'Use #fromVersionString: instead'
		on: '22 September 2017' 
		in: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8'.	
		
	parts := self parseShortVersionString: aString.
	^self new
		type: parts first;
		major: parts second;
		minor: parts third;
		suffix: parts fourth;
		yourself
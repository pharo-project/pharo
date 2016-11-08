initialization
initialize

	super initialize.
	
	Author useAuthor: 'FuzzyMatcherTests' during: [ 
		fuzzyClass := FuzzyMatcher newAnonymousSubclass.
		fuzzyClass 
			compile: 'scoreFor: aString 
				self match: aString ifScored: [ :scr | ^scr ]. ^ nil
			';
			compile: 'scoredMatch: aString
				self match: aString ifScored: [ :scr | ^true ]. ^false
			'.
	]
accessing
privateStyle: aText 
	| styledText |
	aText makeAllColor: Color red.
	styledText := self parser parse: aText asString onError: [  ].
	(styledText notNil and: [ styledText ~~ aText ]) ifTrue: [ 
		self flag: 'change only the runs array of the original text.'.
		aText setString: aText string setRunsChecking: styledText runs ]
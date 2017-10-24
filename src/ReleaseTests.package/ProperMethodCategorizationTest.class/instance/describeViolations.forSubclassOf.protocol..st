utilities
describeViolations: violations forSubclassOf: aClass protocol: aProtocolString
	| offendingClasses selector classList word |
	offendingClasses := violations collect: [ :assoc | assoc key classBinding key ].
	selector := violations first key selector.
	(violations size = 1) ifTrue: [ 
		^ '{1}, a subclass of {2}, should have its #{3} method in protocol ''{4}'''
		    format: {offendingClasses first. aClass asString. selector. aProtocolString} ].
	word := (violations size = 2) ifTrue: [ 'both' ] ifFalse: [ 'all' ].
	classList := 'and ', offendingClasses last.
	1 to: (offendingClasses size - 1) do: [ :i | 
		classList := (offendingClasses at: i) , ', ' , classList  ].
	^ '{1}, {2} subclasses of {3}, should have their #{4} methods in protocol ''{5}'''
		    format: {classList. word. aClass asString. selector. aProtocolString}
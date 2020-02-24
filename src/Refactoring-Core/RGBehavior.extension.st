Extension { #name : #RGBehavior }

{ #category : #'*Refactoring-Core' }
RGBehavior >> sourceCodeAt: aSymbol [

	| method |
	method :=  self localMethodNamed: aSymbol 
						 ifAbsent: [ self error: 'Selector #', aSymbol asString, ' not found in RGClass "', self name, '"' ].
	^ method sourceCode 
]

{ #category : #'*Refactoring-Core' }
RGBehavior >> withAllSubclasses [

	^ self subclasses asOrderedCollection 
		add: self; 
		yourself
]

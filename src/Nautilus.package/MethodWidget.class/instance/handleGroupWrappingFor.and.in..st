private
handleGroupWrappingFor: aSelector and: aStringMorph in: aPackage
	aPackage name = self model model class lastModifiedMethodsGroupName 
		ifFalse: [ ^ aStringMorph ]
		ifTrue: [ 
			aPackage node item methods
				detect: [ :method | method selector = aSelector ]
				ifFound: [ ^ aSelector asMorph ]
				ifNone: [ ^ aStringMorph ] ]
*GT-SpotterExtensions-Core-private
concreteMethodsInContext: aContext usingFilter: aFilter
	" return the concrete methods described by the expression (Class >> #Selector) - evaluate it savely "
	| classAndSelectorStrings class selector |
	classAndSelectorStrings := (aContext textTrimmed splitOn: '>>') collect: [ :each | each trimBoth ].
	
	" check preconditions "
	classAndSelectorStrings size = 2 ifFalse: [ ^ self ].
	classAndSelectorStrings second size < 2 ifTrue: [ ^ self ].

	" retrieve concrete class and selector - or saerch all behaviors "
	selector := classAndSelectorStrings second first = $# 
		ifTrue: [ classAndSelectorStrings second allButFirst asSymbol ]
		ifFalse: [ classAndSelectorStrings second asSymbol ].
	
	(class := Smalltalk classNamed: classAndSelectorStrings first) ifNil: [ 
		SystemNavigation default allBehaviorsDo: [ :aClass |
			aClass methodsDo: [ :method |
				method selector = selector ifTrue: [ aFilter value: method ] ] ].
		^ self ].
	
	
	" retrieve the compiled method in the class and class hierarchy "
	class withAllSuperclassesDo: [ :aClass |
		aFilter value: ([ aClass compiledMethodAt: selector ] on: Error do: [ ^ self ]) ]
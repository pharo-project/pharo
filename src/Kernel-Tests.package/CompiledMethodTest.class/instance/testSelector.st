tests - accessing
testSelector

	Author useAuthor: 'TUTU_TEST' during: [ 
		
		|  method cls |
		
		method := (self class)>>#returnTrue.
		self assert: (method selector = #returnTrue).
		
		"now make an orphaned method. new semantics: return corrent name"	
		Smalltalk removeClassNamed: #TUTU.

		cls := Object subclass: #TUTU
			instanceVariableNames: ''
			classVariableNames: ''
			poolDictionaries: ''
			category: self categoryNameForTemporaryClasses.
		
		cls compile: 'foo ^ 10'.
		
		method := cls >> #foo.
		
		Smalltalk removeClassNamed: #TUTU.

		self assert: method selector = #foo. 
		
	].
	


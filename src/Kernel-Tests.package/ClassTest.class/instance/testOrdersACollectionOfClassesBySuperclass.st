tests - file in/out
testOrdersACollectionOfClassesBySuperclass
	"self debug: #testOrdersACollectionOfClassesBySuperclass"
	| ordered |
	ordered := (Class superclassOrder:  
		(OrderedCollection 
				with: ExampleForTest11 class 
				with: ExampleForTest111 class 
				with: ExampleForTest12 class 
				with: ExampleForTest1 class 
				with: ExampleForTest12 class 
				with: ExampleForTest112 class)).
	
	self assert: (ordered indexOf: ExampleForTest1 class) < (ordered indexOf: ExampleForTest11 class).
	self assert: (ordered indexOf: ExampleForTest11 class) < (ordered indexOf: ExampleForTest111 class).
	self assert: (ordered indexOf: ExampleForTest11 class) < (ordered indexOf: ExampleForTest112 class).
	self assert: (ordered indexOf: ExampleForTest1 class) < (ordered indexOf: ExampleForTest12 class).
	
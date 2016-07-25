tests
testCoverageControlFlow
	selector := #exampleIfTrueIfFalse.

	link := MetaLink new
		metaObject: #node;
		selector: #tagExecuted.
	(ReflectivityExamples >> #exampleIfTrueIfFalse) ast nodesDo: [ :node | node link: link ].
	self assert: ReflectivityExamples new exampleIfTrueIfFalse = 5.
	self deny: ((ReflectivityExamples >> #exampleIfTrueIfFalse) ast sendNodes select: [ :each | each selector = 'bar' ]) first hasBeenExecuted.
	
	self deny:  (ReflectivityExamples >> #exampleIfTrueIfFalse) blockNodes first hasBeenExecuted. self flag: #TBD. "should be true. need to implement links on optimized block bodies"
	self deny:  (ReflectivityExamples >> #exampleIfTrueIfFalse) blockNodes first body hasBeenExecuted.
	
	self assert: (ReflectivityExamples >> #exampleIfTrueIfFalse) blockNodes second hasBeenExecuted.
	self assert: (ReflectivityExamples >> #exampleIfTrueIfFalse) blockNodes second body hasBeenExecuted.
	self assert: (ReflectivityExamples >> #exampleIfTrueIfFalse) ast hasBeenExecuted.
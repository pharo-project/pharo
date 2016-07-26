failure tests
testPullUpClassMethod
	| class |
	class := model classNamed: #Object.
	class addClassVariable: #Foo.
	class theMetaClass 
		compile: 'foo ^Foo'
		classified: #(#accessing ).
	self shouldFail: (RBPullUpMethodRefactoring 
			model: model
			pullUp: #(#foo )
			from: class theMetaClass)
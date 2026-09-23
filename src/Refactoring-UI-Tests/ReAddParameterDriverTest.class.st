"
I test ReAddParameterDriver. I verify that the driver gathers the new signature, validates it and runs RBAddParameterRefactoring.
"
Class {
	#name : 'ReAddParameterDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
ReAddParameterDriverTest >> testAddParameterCollidingSelectorInformsUser [
	"Adding an argument that produces a selector already defined in the class must inform the user and not override it."

	| rbclasses model method driver dialog colliding valid |
	rbclasses := RBClassEnvironment classes: { RBClassDataForRefactoringTest }.
	model := RBNamespace onEnvironment: rbclasses.
	method := (model classObjectFor: RBClassDataForRefactoringTest) methodFor: #called:on:.
	driver := ReAddParameterDriver new
		          scopes: { rbclasses }
		          model: model
		          addParameterToMethodSignature: method
		          in: RBClassDataForRefactoringTest.
	self setUpDriver: driver.
	driver application: StApplicationForRefactorDriverMock new.

	colliding := RBMethodName selector: #textInput:name:symbol: arguments: #( 'a' 'b' 'c' ).
	colliding
		permutation: #( 1 2 -1 );
		newArgs: { (RBArgumentName name: 'extra' value: 'nil') }.
	valid := RBMethodName selector: #called:extra:on: arguments: #( 'a' 'extra' 'b' ).
	valid
		permutation: #( 1 -1 2 );
		newArgs: { (RBArgumentName name: 'extra' value: 'nil') }.

	dialog := MockObject new.
	dialog on: #cancelled respond: false.
	dialog on: #methodName respond: colliding.
	dialog on: #cancelled respond: false.
	dialog on: #methodName respond: valid.
	driver requestDialog: dialog.
	driver runRefactoring.

	self assert: (driver application shownMessage includesSubstring: 'already defined').
	self assert: ((driver refactoring model classObjectFor: RBClassDataForRefactoringTest) definesMethod: #called:extra:on:).
	self assert: ((driver refactoring model classObjectFor: RBClassDataForRefactoringTest) definesMethod: #textInput:name:symbol:)
]

{ #category : 'tests' }
ReAddParameterDriverTest >> testAddParameterExpectSuccess [
	| rbclasses model method driver dialog methodName |
	
	rbclasses := RBClassEnvironment classes: { MyClassARoot }.
	model := RBNamespace onEnvironment: rbclasses.
	method := (model classObjectFor: MyClassARoot) methodFor: 'accessingSharedVariable' asSymbol.
	driver := ReAddParameterDriver new
		scopes: { rbclasses }
		model: model
		addParameterToMethodSignature: method
		in: MyClassARoot.
	self setUpDriver: driver.
	
	methodName := RBMethodName selector: #accessingSharedVariable: arguments: #( 'anObject' ).
	methodName newArgs: { (RBArgumentName name: 'anObject' value: 'nil') }.
	dialog := MockObject new.
	dialog on: #cancelled respond: false.
	dialog on: #methodName respond: methodName.
	driver requestDialog: dialog.
	driver runRefactoring.

	self 
		assert: driver refactoring model changes changes size 
		equals: 6
]

{ #category : 'tests' }
ReAddParameterDriverTest >> testInvalidInitializationValueFollowedByValidExpectSuccess [
	| rbclasses model method driver dialog invalidMethodName validMethodName |
	
	rbclasses := RBClassEnvironment classes: { MyClassARoot }.
	model := RBNamespace onEnvironment: rbclasses.
	method := (model classObjectFor: MyClassARoot) methodFor: 'accessingSharedVariable' asSymbol.
	
	driver := ReAddParameterDriver new
		scopes: { rbclasses }
		model: model
		addParameterToMethodSignature: method
		in: MyClassARoot.
	self setUpDriver: driver.
	invalidMethodName := RBMethodName selector: #accessingSharedVariable: arguments: #( 'anObject' ).
	invalidMethodName newArgs: { (RBArgumentName name: 'anObject' value: '1 +') }.
	validMethodName := RBMethodName selector: #accessingSharedVariable: arguments: #( 'anObject' ).
	validMethodName newArgs: { (RBArgumentName name: 'anObject' value: 'nil') }.
	dialog := MockObject new.
	dialog on: #cancelled respond: false.
	dialog on: #methodName respond: invalidMethodName.
	dialog on: #cancelled respond: false.
	dialog on: #methodName respond: validMethodName.
	driver requestDialog: dialog.
	driver runRefactoring.

	self 
		assert: driver refactoring model changes changes size 
		equals: 6
]

{ #category : 'tests' }
ReAddParameterDriverTest >> testReorderArgumentsRenamesMethod [
	"Reordering the non-leading arguments produces a renamed method: the
	keyword parts follow the arguments (same arity, no added argument)."

	| rbclasses model method driver dialog reorderedMessage class |
	rbclasses := RBClassEnvironment classes: { RBClassDataForRefactoringTest }.
	model := RBNamespace onEnvironment: rbclasses.
	method := (model classObjectFor: RBClassDataForRefactoringTest) methodFor: #textInput:name:symbol:.
	driver := ReAddParameterDriver new
		          scopes: { rbclasses }
		          model: model
		          addParameterToMethodSignature: method
		          in: RBClassDataForRefactoringTest.
	self setUpDriver: driver.
	reorderedMessage := RBMethodName selector: #textInput:symbol:name: arguments: #( 'html' 'aSymbol' 'aString' ).
	reorderedMessage permutation: (OrderedCollection with: 1 with: 3 with: 2).
	dialog := MockObject new.
	dialog on: #cancelled respond: false.
	dialog on: #methodName respond: reorderedMessage.
	driver requestDialog: dialog.
	driver runRefactoring.
	class := driver refactoring model classObjectFor: RBClassDataForRefactoringTest.
	self assert: (class definesMethod: #textInput:symbol:name:).
	self deny: (class definesMethod: #textInput:name:symbol:).
	self assert: (class parseTreeForSelector: #textInput:symbol:name:) argumentNames asArray equals: #( html aSymbol aString )
]

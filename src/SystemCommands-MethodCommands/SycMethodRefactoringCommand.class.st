"
I am the root of the refactorings of the selected methods, such as pull up, push down or the moves to another class, side or package.
I add the refactoring scopes and the refactoring model to SycMethodCommand. My subclasses usually answer their refactorings with #asRefactorings, which I execute.
#for: answers a command already prepared for some methods, to run it outside of a tool.
"
Class {
	#name : 'SycMethodRefactoringCommand',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'refactoringScopes'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'instance creation' }
SycMethodRefactoringCommand class >> for: methods [

	^ self new
		  methods: methods;
		  prepared: true;
		  yourself
]

{ #category : 'testing' }
SycMethodRefactoringCommand >> canBeExecuted [

	^ self methods isNotEmpty
]

{ #category : 'execution' }
SycMethodRefactoringCommand >> executeRefactoring [

	self asRefactorings do: [ :each | each execute ]
]

{ #category : 'preparation' }
SycMethodRefactoringCommand >> prepareFullExecution [
	super prepareFullExecution.
	model ifNil: [ self setUpModelFromContext: self context ]
]

{ #category : 'accessing' }
SycMethodRefactoringCommand >> refactoringScopes [

	^ refactoringScopes ifNil: [
			  self hasContext
				  ifTrue: [ self context refactoringScopes ]
				  ifFalse: [ #(  ) ] ]
]

{ #category : 'accessing' }
SycMethodRefactoringCommand >> refactoringScopes: aCollection [

	refactoringScopes := aCollection
]

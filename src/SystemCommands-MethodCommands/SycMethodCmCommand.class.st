"
This class is an extension so we can mix command of Commander2 with Commander.
"
Class {
	#name : 'SycMethodCmCommand',
	#superclass : 'SycCmCommand',
	#instVars : [
		'methods',
		'refactoringScopes'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'adding' }
SycMethodCmCommand class >> activationStrategy [
	^ SycMethodMenuActivation
]

{ #category : 'testing' }
SycMethodCmCommand class >> canBeExecutedInContext: aToolContext [
	^ (self new
		context: aToolContext;
		yourself) canBeExecuted
]

{ #category : 'instance creation' }
SycMethodCmCommand class >> for: methods [

	^ self new
		  methods: methods;
		  prepared: true;
		  yourself
]

{ #category : 'testing' }
SycMethodCmCommand >> canBeExecuted [
	^ self methods isNotEmpty
]

{ #category : 'execution' }
SycMethodCmCommand >> execute [

	prepared
		ifTrue: [ self executeRefactoring ]
		ifFalse: [
				[
					prepared := true.
					self prepareFullExecution.
					self executeRefactoring.
					self applyCommandResult ]
					on: Exception
					do: [ :exception | self processCommandFailure: exception ] ]
]

{ #category : 'execution' }
SycMethodCmCommand >> executeRefactoring [

	self asRefactorings do: [ :each | each execute ]
]

{ #category : 'accessing' }
SycMethodCmCommand >> methods [

	^ methods ifNil: [
			  self hasContext
				  ifTrue: [ self context selectedMethods ]
				  ifFalse: [ #(  ) ] ]
]

{ #category : 'accessing' }
SycMethodCmCommand >> methods: aCollection [

	methods := aCollection
]

{ #category : 'preparation' }
SycMethodCmCommand >> prepareFullExecution [
	super prepareFullExecution.
	model ifNil: [ self setUpModelFromContext: self context ]
]

{ #category : 'accessing' }
SycMethodCmCommand >> refactoringScopes [

	^ refactoringScopes ifNil: [
			  self hasContext
				  ifTrue: [ self context refactoringScopes ]
				  ifFalse: [ #(  ) ] ]
]

{ #category : 'accessing' }
SycMethodCmCommand >> refactoringScopes: aCollection [

	refactoringScopes := aCollection
]

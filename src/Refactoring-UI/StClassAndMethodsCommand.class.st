Class {
	#name : 'StClassAndMethodsCommand',
	#superclass : 'CmCommand',
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'private' }
StClassAndMethodsCommand >> classMethodsTable [ 
	"Answer the presenter's <SpTablePresenter> that holds the class methods"

	^ 	self context classMethodsTable.
]

{ #category : 'private' }
StClassAndMethodsCommand >> classMethodsTableItems [
	"Answer a <Collection> of presenter's <CompiledMethods> representing the class methods"

	^ 	self classMethodsTable items
]

{ #category : 'private' }
StClassAndMethodsCommand >> instanceMethodsTable [
	"Answer the presenter's <SpTablePresenter> that holds the instance methods"

	^ 	self context instanceMethodsTable
]

{ #category : 'private' }
StClassAndMethodsCommand >> instanceMethodsTableItems [
	"Answer a <Collection> of presenter's <CompiledMethods> representing the instance methods"

	^ 	self instanceMethodsTable items
]

{ #category : 'testing' }
StClassAndMethodsCommand >> isInstanceMethodsSelected [
	"Answer <true> if the receiver's tool is displaying the instance methods page"
	
	^ self context isInstanceMethodsSelected
]

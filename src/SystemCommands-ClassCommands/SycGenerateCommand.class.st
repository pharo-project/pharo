"
I am the root of the commands which generate methods in the selected class with a refactoring driver, such as #= and #hash or #printOn:.
My subclasses answer the driver with #driverClass.
"
Class {
	#name : 'SycGenerateCommand',
	#superclass : 'SycClassRefactoringCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'testing' }
SycGenerateCommand >> canBeExecuted [
	"Answer <true> if the receiver has any variables"

	^ false
]

{ #category : 'executing' }
SycGenerateCommand >> driverClass [
	^ self subclassResponsibility
]

{ #category : 'executing' }
SycGenerateCommand >> executeRefactoring [

	(self driverClass new
		 targetClass: targetClass name;
		 scopes: refactoringScopes) runRefactoring
]

{ #category : 'accessing' }
SycGenerateCommand >> name [

	^ self driverClass menuItemString
]

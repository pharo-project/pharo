"
Abstract class that groups Commander 2 behavior related to method generation.

Subclasses must implement `driverClass` answering the driver which will be executed by running the command.
"
Class {
	#name : 'SycCmGenerateCommand',
	#superclass : 'SycClassCmCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'executing' }
SycCmGenerateCommand >> driverClass [
	^ self subclassResponsibility
]

{ #category : 'executing' }
SycCmGenerateCommand >> executeRefactoring [

	(self driverClass new
		 targetClass: targetClass name;
		 scopes: refactoringScopes) runRefactoring
]

{ #category : 'testing' }
SycCmGenerateCommand >> isApplicable [
	"Answer <true> if the receiver has any variables"

	^ false
]

{ #category : 'accessing' }
SycCmGenerateCommand >> name [

	^ self driverClass menuItemString
]

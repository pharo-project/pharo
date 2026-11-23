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
SycGenerateCommand class >> isAbstract [

	^ self == SycGenerateCommand
]

{ #category : 'testing' }
SycGenerateCommand >> canBeExecuted [
	"Whether the selected class has variables to generate code from."

	^ context lastSelectedClass realClass slots isNotEmpty
]

{ #category : 'executing' }
SycGenerateCommand >> driverClass [
	^ self subclassResponsibility
]

{ #category : 'execution' }
SycGenerateCommand >> executeRefactoring [

	(self driverClass new
		 targetClass: targetClass name;
		 scopes: refactoringScopes) runRefactoring
]

{ #category : 'accessing' }
SycGenerateCommand >> name [

	^ self driverClass menuItemString
]

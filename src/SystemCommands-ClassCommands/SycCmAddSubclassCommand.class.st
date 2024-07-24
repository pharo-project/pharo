"
I am a command to create new subclass og given class (my targetClass)
"
Class {
	#name : 'SycCmAddSubclassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'executing' }
SycCmAddSubclassCommand >> executeRefactoring [
	"The class name is necessary so the browser can select the new subclass"
	| driver |

	driver := (ReAddSubclassDriver 
		superclass: self targetClass 
		packageNames: self packageNames)
			scopes: refactoringScopes.
	driver subclass
		ifNil: [ ^ self ]. " cancelled "
	driver runRefactoring.
	newClassName := driver subclass.
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> icon [
	^self iconNamed:#add
]

{ #category : 'testing' }
SycCmAddSubclassCommand >> isApplicable [

	^ context lastSelectedClass isTrait not
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> name [
	^'New subclass'
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> newClassName [

	^ newClassName 
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> newClassName: aString [ 

	newClassName := aString asSymbol
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> order [
	^ 10100.1
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> packageNames [
	"Answer a <Collection> of <Symbol> of the packages available to the new class"
	
	^ PackageOrganizer default packageNames
]

{ #category : 'preparation' }
SycCmAddSubclassCommand >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.

]

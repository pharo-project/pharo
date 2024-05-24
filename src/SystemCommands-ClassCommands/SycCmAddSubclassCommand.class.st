"
I am a command to create new subclass og given class (my targetClass)
"
Class {
	#name : 'SycCmAddSubclassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#instVars : [
		'packageName'
	],
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'executing' }
SycCmAddSubclassCommand >> executeRefactoring [
	"The class name is necessary so the browser can select the new subclass"
	| refactoring |

	refactoring := (ReAddSubclassDriver 
		superclass: self targetClass 
		packageNames: self packageNames)
			scopes: refactoringScopes.
	refactoring subclass
		ifNil: [ ^ self ]. " cancelled "
	refactoring runRefactoring.
	newClassName := refactoring subclass.
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

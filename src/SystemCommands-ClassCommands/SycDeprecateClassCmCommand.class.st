Class {
	#name : 'SycDeprecateClassCmCommand',
	#superclass : 'SycClassCmCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycDeprecateClassCmCommand >> executeRefactoring [

	(RBDeprecateClassDriver
		 className: targetClass name
		 scopes: refactoringScopes) runRefactoring
]

{ #category : 'testing' }
SycDeprecateClassCmCommand >> isApplicable [

	^ context lastSelectedClass realClass isDeprecated not
]

{ #category : 'accessing' }
SycDeprecateClassCmCommand >> name [

	^ 'Deprecate'
]

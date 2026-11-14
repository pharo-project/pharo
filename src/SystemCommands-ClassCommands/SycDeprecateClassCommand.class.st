"
Deprecate the selected class.

I am disabled for now (see #canBeExecuted) because the deprecation is not working.
"
Class {
	#name : 'SycDeprecateClassCommand',
	#superclass : 'SycClassRefactoringCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'testing' }
SycDeprecateClassCommand >> canBeExecuted [

	self flag: 'Disabled for Pharo14, as it is not working'.

	"^ context lastSelectedClass realClass isDeprecated not"
	
	^ false
]

{ #category : 'accessing' }
SycDeprecateClassCommand >> name [

	^ 'Deprecate'
]

Class {
	#name : 'SycDeprecateClassCmCommand',
	#superclass : 'SycClassCmCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'testing' }
SycDeprecateClassCmCommand >> canBeExecuted [

	^ context lastSelectedClass realClass isDeprecated not
]

{ #category : 'accessing' }
SycDeprecateClassCmCommand >> name [

	^ 'Deprecate'
]

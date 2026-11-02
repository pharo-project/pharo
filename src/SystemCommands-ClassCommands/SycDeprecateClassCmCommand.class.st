Class {
	#name : 'SycDeprecateClassCmCommand',
	#superclass : 'SycClassCmCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'testing' }
SycDeprecateClassCmCommand >> isApplicable [

	self flag: 'Disabled for Pharo14, as it is not working'.

	"^ context lastSelectedClass realClass isDeprecated not"
	
	^ false
]

{ #category : 'accessing' }
SycDeprecateClassCmCommand >> name [

	^ 'Deprecate'
]

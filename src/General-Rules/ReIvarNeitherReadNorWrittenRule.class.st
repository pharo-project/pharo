"
This smell arises when an instance variable is not both read and written. If an instance variable is only read, the reads can be replaced by nil, since it could not have been assigned a value. If the variable is only written, then it does not need to store the result since it is never used.

If you really intend to have an unused variable (for example, for testing purpose), you can add a #ignoreUnusedVariables: pragma to your initialize method with an array of unused variables. Example: `<ignoreUnusedVariables: #( instVar1 instVar2 )>`.
"
Class {
	#name : 'ReIvarNeitherReadNorWrittenRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Clean Code',
	#package : 'General-Rules',
	#tag : 'Clean Code'
}

{ #category : 'testing-interest' }
ReIvarNeitherReadNorWrittenRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReIvarNeitherReadNorWrittenRule class >> group [
	^ 'Clean Code'
]

{ #category : 'accessing' }
ReIvarNeitherReadNorWrittenRule class >> ruleName [
	^ 'Instance variable not read or not written'
]

{ #category : 'accessing' }
ReIvarNeitherReadNorWrittenRule class >> severity [
	^ #information
]

{ #category : 'running' }
ReIvarNeitherReadNorWrittenRule >> check: aClass forCritiquesDo: aCriticBlock [

	aClass slots
		reject: [ :slot |
			slot isReferenced or: [
				slot definingClass pragmas anySatisfy: [ :pragma | pragma selector = #ignoreUnusedVariables: and: [ (pragma argumentAt: 1) includes: slot name ] ] ] ]
		thenDo: [ :slot | aCriticBlock cull: (self critiqueFor: aClass about: slot name) ]
]

"
I am a refactoring for protecting instance variable access.

If a class defines methods for reading and writing instance variables, they are removed and all calls on this methods.
Omit method that are redefined in subclasses.
"
Class {
	#name : #RBProtectInstanceVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'private-accessing' }
RBProtectInstanceVariableRefactoring >> getterSetterMethods [
	| matcher |
	matcher := self parseTreeSearcher.
	matcher
		answer: Set new;
		matchesAnyMethodOf:
			(Array
				with: '`method ^' , variableName
				with: ('`method: `arg <1s> := `arg' expandMacrosWith: variableName)
				with: ('`method: `arg ^<1s> := `arg' expandMacrosWith: variableName))
			do: [ :aNode :answer | 
				(class subclassRedefines: aNode selector)
					ifFalse: [ answer add: aNode selector ].
				answer ].
	(class whichSelectorsReferToInstanceVariable: variableName)
		do:
			[ :each | self checkClass: class selector: each using: matcher ].
	^ matcher answer
]

{ #category : #transforming }
RBProtectInstanceVariableRefactoring >> inline: aSelector [ 
	self onError: 
			[self performComponentRefactoring: (RBInlineAllSendersRefactoring 
						model: self model
						sendersOf: aSelector
						in: class)]
		do: []
]

{ #category : #preconditions }
RBProtectInstanceVariableRefactoring >> preconditions [
	^RBCondition definesInstanceVariable: variableName in: class
]

{ #category : #transforming }
RBProtectInstanceVariableRefactoring >> transform [
	self setOption: #inlineExpression toUse: [:ref :string | true].
	self getterSetterMethods do: [:each | self inline: each]
]

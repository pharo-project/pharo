"
I am a refactoring for protecting instance variable access.  

When doing a transformation on the source code, I will check for accessors of instance variable and inline all calls to the method calling them. 
I take subclasses and sibling classes into account, preventing breaking changes.
Also if any subclass redefines an accessor for the variable, I will not apply changes for it. 
"
Class {
	#name : 'ReProtectInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
ReProtectInstanceVariableRefactoring >> applicabilityPreconditions [

	^ { (ReDirectlyDefinesInstanceVariableCondition
		   classNamed: class name
		   inModel: self model
		   instanceVariables: { variableName }) }
]

{ #category : 'private - accessing' }
ReProtectInstanceVariableRefactoring >> getterSetterMethods [
	"Getter/setters are methods are accessors that are not redefined in subclasses."
	
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

{ #category : 'transforming' }
ReProtectInstanceVariableRefactoring >> inline: aSelector [
	self onError:
			[self generateChangesFor: (ReInlineIntoLocalSendersRefactoring
						model: self model
						sendersOf: aSelector
						in: class)]
		do: []
]

{ #category : 'transforming' }
ReProtectInstanceVariableRefactoring >> privateTransform [
	self setOption: #shouldNotCreateExtraBindings toUse: [:ref :string | true].
	self getterSetterMethods do: [:each | self inline: each]
]

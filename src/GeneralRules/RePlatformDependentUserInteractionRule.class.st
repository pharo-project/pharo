"
Check the methods that  use platform dependent user interactions.

This rule should be updated.
"
Class {
	#name : #RePlatformDependentUserInteractionRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RePlatformDependentUserInteractionRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'PlatformDependentUserInteractionRule'
]

{ #category : #accessing }
RePlatformDependentUserInteractionRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RePlatformDependentUserInteractionRule >> initialize [
	super initialize.
	 self matchesAny: #(
			'PopUpMenu confirm: `@object1 orCancel: `@object2'
			'PopUpMenu confirm: `@object1 trueChoice: `@object2 falseChoice: `@object3'
			'PopUpMenu confirm: `@object1'
			'PopUpMenu inform: `@object1'
			'PopUpMenu initialize'
			'PopUpMenu labelArray: `@object1 lines: `@object2'
			'PopUpMenu labelArray: `@object1'
			'PopUpMenu labels: `@object1 lines: `@object2'
			'PopUpMenu labels: `@object1'
			'PopUpMenu withCaption: `@object1 chooseFrom: `@object2'
			 )
]

{ #category : #accessing }
RePlatformDependentUserInteractionRule >> name [
	^ 'Platform dependent user interaction'
]

{ #category : #accessing }
RePlatformDependentUserInteractionRule >> rationale [
	^ 'Check the methods that  use platform dependent user interactions.'
]

{ #category : #accessing }
RePlatformDependentUserInteractionRule >> severity [
	^ #error
]

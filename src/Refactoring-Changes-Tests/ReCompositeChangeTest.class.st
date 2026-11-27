Class {
	#name : 'ReCompositeChangeTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'tests' }
ReCompositeChangeTest >> testIsInScopeKeepsUnboundRename [

	| change env |
	change := ReRenameClassChange rename: #ReMissingOldName to: #ReMissingNewName.
	env := RBClassEnvironment classes: { Object }.

	self assert: change changeClass isNil.
	self assert: change targetsNewClass.
	self assert: (change isInScope: env)
]

{ #category : 'tests' }
ReCompositeChangeTest >> testRenameOfExistingClassFollowsScope [

	| change |
	change := ReRenameClassChange rename: #Point to: #RePointRenameProbe.

	self deny: change targetsNewClass.
	self assert: (change isInScope: (RBClassEnvironment classes: { Point })).
	self deny: (change isInScope: (RBClassEnvironment classes: { String }))
]

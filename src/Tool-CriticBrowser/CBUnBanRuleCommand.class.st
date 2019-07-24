Class {
	#name : #CBUnBanRuleCommand,
	#superclass : #CBRuleCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBUnBanRuleCommand >> execute [
	self selectedRule isComposite
		ifTrue: [ self selectedRule rules do: [ :r | self context allPackages do: [ :package | self context removeFalsePositiveRule: r forPackage: package ] ] ]
		ifFalse: [ self context allPackages do: [ :package | self context removeFalsePositiveRule: self selectedRule forPackage: package ] ]
]

{ #category : #initialization }
CBUnBanRuleCommand >> initialize [
	super initialize.
	self
		basicName: 'Unban from selected packages';
		basicDescription: 'Unban from selected packages it`s like unmark as a false positive.'
]

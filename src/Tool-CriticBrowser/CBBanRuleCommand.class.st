Class {
	#name : #CBBanRuleCommand,
	#superclass : #CBRuleCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBBanRuleCommand >> execute [
	self selectedRule isComposite
		ifTrue: [ self selectedRule rules do: [ :aRule | self context allPackages do: [ :package | self context addFalsePositiveRule: aRule forPackage: package ] ] ]
		ifFalse: [ self context allPackages do: [ :package | self context addFalsePositiveRule: self selectedRule forPackage: package ] ]
]

{ #category : #initialization }
CBBanRuleCommand >> initialize [
	super initialize.
	self
		basicName: 'Ban for selected packages';
		basicDescription: 'Banning a rule is equivalent to mark it as a false positive.'
]

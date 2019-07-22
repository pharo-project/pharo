Class {
	#name : #CBBanRuleMenu,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-utilities-contextMenu'
}

{ #category : #hooks }
CBBanRuleMenu >> execute [
	"Execute the actions that should be done by the command.
	 This method expect that the context has been put in #context inst. var. if any context is relevant."

	self context selectedRule
		ifNotNil: [ :rule | 
			rule isComposite
				ifTrue: [ rule rules
						do: [ :aRule | 
							self context allPackages
								do:
									[ :package | self context addFalsePositiveRule: aRule forPackage: package ] ] ]
				ifFalse: [ self context allPackages
						do:
							[ :package | self context addFalsePositiveRule: rule forPackage: package ] ] ]
]

{ #category : #initialization }
CBBanRuleMenu >> initialize [
	super initialize.
	self
		basicName: 'ban rule';
		basicDescription: 'ban rule is equivalent to mark as FalsePositive all critic if this rule'
]

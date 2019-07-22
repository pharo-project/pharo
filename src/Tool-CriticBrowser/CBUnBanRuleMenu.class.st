Class {
	#name : #CBUnBanRuleMenu,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-utilities-contextMenu'
}

{ #category : #hooks }
CBUnBanRuleMenu >> execute [
	self context selectedRule
		ifNotNil: [ :rule | 
			rule isComposite
				ifTrue: [ rule rules
						do: [ :r | 
							self context allPackages
								do:
									[ :package | self context removeFalsePositiveRule: r forPackage: package ] ] ]
				ifFalse: [ self context allPackages
						do:
							[ :package | self context removeFalsePositiveRule: rule forPackage: package ] ] ]
]

{ #category : #initialization }
CBUnBanRuleMenu >> initialize [
	super initialize.
	self
		basicName: 'unBan selectedRule';
		basicDescription: 'Unban from selected packages it`s like unMark as FalsePositive'
]

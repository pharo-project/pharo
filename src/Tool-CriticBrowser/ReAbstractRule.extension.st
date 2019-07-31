Extension { #name : #ReAbstractRule }

{ #category : #'*Tool-CriticBrowser' }
ReAbstractRule >> CBCommandExecuteReapplyRuleOn: aCBCommand [
	aCBCommand context reapplyRule: self
]

{ #category : #'*Tool-CriticBrowser' }
ReAbstractRule >> CBCommandExecuteUnBanRule: anInstanceOfCBUnBanRuleCommand [
	anInstanceOfCBUnBanRuleCommand context allPackages
		do: [ :package | 
			anInstanceOfCBUnBanRuleCommand context
				removeFalsePositiveRule: self
				forPackage: package ]
]

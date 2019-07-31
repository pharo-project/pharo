Extension { #name : #ReAbstractRule }

{ #category : #'*Tool-CriticBrowser' }
ReAbstractRule >> CBCommandExecuteOn: aCBCommand [
	aCBCommand context reapplyRule: self
]

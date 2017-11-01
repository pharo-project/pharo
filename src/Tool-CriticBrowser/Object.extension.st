Extension { #name : #Object }

{ #category : #'*Tool-CriticBrowser' }
Object >> populateCBMenu: aMenu [

	aMenu add: 'Browse' target: self selector: #browse
]

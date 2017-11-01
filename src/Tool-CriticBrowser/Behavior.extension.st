Extension { #name : #Behavior }

{ #category : #'*Tool-CriticBrowser' }
Behavior >> populateCBMenu: aMenu [
	
	super populateCBMenu: aMenu.
	aMenu
		add: 'References to...'
		target: SystemNavigation new
		selector: #browseAllCallsOnClass:
		argument: self
]

Extension { #name : #Trait }

{ #category : #'*Tool-CriticBrowser' }
Trait >> populateCBMenu: aMenu [
	
	super populateCBMenu: aMenu.
	aMenu
		add: 'Users of...'
		target: SystemNavigation new
		selector: #browseAllUsersOfTrait:
		argument: self
]

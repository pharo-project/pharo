Extension { #name : #TraitBehavior }

{ #category : #'*Tool-CriticBrowser' }
TraitBehavior >> populateCBMenu: aMenu [
	
	super populateCBMenu: aMenu.
	aMenu
		add: 'Users of...'
		target: SystemNavigation new
		selector: #browseAllUsersOfTrait:
		argument: self
]

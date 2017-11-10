Extension { #name : #CompiledMethod }

{ #category : #'*Tool-CriticBrowser' }
CompiledMethod >> populateCBMenu: aMenu [
	
	super populateCBMenu: aMenu.
	aMenu
		add: 'Implementors of...'
		target: SystemNavigation new
		selector: #browseAllImplementorsOf:
		argument: self selector;
		
		add: 'Senders of...'
		target: SystemNavigation new
		selector: #browseAllSendersOf:
		argument: self selector
]

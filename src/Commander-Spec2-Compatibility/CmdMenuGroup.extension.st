Extension { #name : #CmdMenuGroup }

{ #category : #'*Commander-Spec2-Compatibility' }
CmdMenuGroup >> addToMenuPresenter: aSpMenuPresenter [
	| subMenuPresenter |
	subMenuPresenter := SpMenuPresenter new.
	self contents
		do: [ :each | each addToMenuPresenter: subMenuPresenter ].
	aSpMenuPresenter
		addItem: [ :item | 
			item
				name: self name;
				icon: self icon;
				subMenu: subMenuPresenter ]
]

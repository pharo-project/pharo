Extension { #name : #CmUIDisplayAsSubMenu }

{ #category : #'*Spec2-Commander2' }
CmUIDisplayAsSubMenu >> display: aCmSpecCommandGroup in: aMenuPresenter do: aBlock [

	aMenuPresenter addItem: [ :menuItem |
		menuItem subMenu: (self 
			fillSubMenuIn: menuItem 
			with: aCmSpecCommandGroup 
			do: aBlock) ]
]

{ #category : #'*Spec2-Commander2' }
CmUIDisplayAsSubMenu >> fillSubMenuIn: menuItem with: aCmSpecCommandGroup do: aBlock [
	| subMenu |
	
	menuItem
		name: aCmSpecCommandGroup name;
		description: aCmSpecCommandGroup description;
		icon: aCmSpecCommandGroup icon.
	subMenu := SpMenuPresenter new.
	aBlock value: subMenu.
	^ subMenu
]

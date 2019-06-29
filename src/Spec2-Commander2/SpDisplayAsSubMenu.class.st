"
I display a command group as a submenu in the MenuPresenter.
"
Class {
	#name : #SpDisplayAsSubMenu,
	#superclass : #SpCommandGroupDisplayStrategy,
	#category : #'Spec2-Commander2-Core'
}

{ #category : #displaying }
SpDisplayAsSubMenu >> display: aCmSpecCommandGroup in: aMenuPresenter do: aBlock [

	aMenuPresenter addItem: [ :menuItem |
		menuItem subMenu: (self 
			fillSubMenuIn: menuItem 
			with: aCmSpecCommandGroup 
			do: aBlock) ]
]

{ #category : #private }
SpDisplayAsSubMenu >> fillSubMenuIn: menuItem with: aCmSpecCommandGroup do: aBlock [
	| subMenu |
	
	menuItem
		name: aCmSpecCommandGroup name;
		description: aCmSpecCommandGroup description;
		icon: aCmSpecCommandGroup icon.
	subMenu := SpMenuPresenter new.
	aBlock value: subMenu.
	^ subMenu
]

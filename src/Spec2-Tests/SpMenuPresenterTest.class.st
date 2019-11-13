Class {
	#name : #SpMenuPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpMenuPresenterTest >> classToTest [
	^ SpMenuPresenter
]

{ #category : #tests }
SpMenuPresenterTest >> testFromSpecSingle [
	| menuDescription |
	menuDescription := PluggableMenuSpec withModel: nil.
	menuDescription add: 'item'.
	presenter fromSpec: menuDescription.
	self assert: presenter menuGroups size equals: 1.
	self assert: presenter menuGroups first menuItems size equals: 1.
	self assert: presenter menuGroups first menuItems first name equals: 'item'.
	self openInstance
]

{ #category : #tests }
SpMenuPresenterTest >> testFromSpecSubMenu [
	| root subMenuRoot parent subMenu |
	root := PluggableMenuSpec withModel: nil.
	parent := root add: 'parent'.
	subMenuRoot := PluggableMenuSpec withModel: nil.
	subMenuRoot add: 'child'.
	parent subMenu: subMenuRoot.
	presenter fromSpec: root.
	subMenu := presenter menuGroups first menuItems first subMenu.
	self assert: subMenu menuGroups size equals: 1.
	self assert: subMenu menuGroups first menuItems size equals: 1.
	self assert: subMenu menuGroups first menuItems first name equals: 'child'.
	self openInstance
]

{ #category : #tests }
SpMenuPresenterTest >> testValue [ 

	self assert: presenter value equals: presenter
]

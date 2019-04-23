Class {
	#name : #MenuPresenterTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
MenuPresenterTest >> classToTest [
	^ MenuPresenter
]

{ #category : #tests }
MenuPresenterTest >> testFromSpecSingle [
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
MenuPresenterTest >> testFromSpecSubMenu [
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

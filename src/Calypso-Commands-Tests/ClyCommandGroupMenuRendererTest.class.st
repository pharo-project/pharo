"
Tests for ClyCommandGroupMenuRenderer.
"
Class {
	#name : 'ClyCommandGroupMenuRendererTest',
	#superclass : 'TestCase',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'tests' }
ClyCommandGroupMenuRendererTest >> testRendersOneItemPerCommand [

	| group menu |
	group := ClyCommandGroupBuilder
		         buildFor: ClyMockBaseCommand
		         context: nil.
	menu := MenuMorph new.
	ClyCommandGroupMenuRenderer render: group on: menu.
	self assert: menu items size equals: 2
]

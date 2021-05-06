Class {
	#name : #SpChooserPresenterTest,
	#superclass : #TestCase,
	#instVars : [
		'chooserPresenter'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #accessing }
SpChooserPresenterTest >> packages [
	^ {(RPackageOrganizer default packages sorted: #packageName ascending) . #packageName}
]

{ #category : #running }
SpChooserPresenterTest >> setUp [
	super setUp.
	chooserPresenter := SpChooserPresenter on: self packages.
]

{ #category : #running }
SpChooserPresenterTest >> testAddAll [
	
	self assert: chooserPresenter list unfilteredItems isNotEmpty.
	self assert: chooserPresenter selectedList unfilteredItems isEmpty.
	chooserPresenter addAllButton click.
	self assert: chooserPresenter list unfilteredItems isEmpty.
	self assert: chooserPresenter selectedList unfilteredItems isNotEmpty.
]

{ #category : #running }
SpChooserPresenterTest >> testAddItem [
	
	self assert: chooserPresenter list selectedItems isEmpty.
	chooserPresenter list selectItem: chooserPresenter list unfilteredItems first.
	chooserPresenter addButton click.
	
	self assert: chooserPresenter selectedList unfilteredItems size equals: 1.
]

{ #category : #running }
SpChooserPresenterTest >> testFilterList [
	| sizeList |
	sizeList := chooserPresenter list items size.
	chooserPresenter list applyFilter: 'ref'.
	self assert: sizeList > chooserPresenter list items size.
	chooserPresenter addAllButton click.
	sizeList := chooserPresenter selectedList items size.
	chooserPresenter selectedList applyFilter: 'ref'.
	self assert: sizeList > chooserPresenter selectedList items size.
]

{ #category : #running }
SpChooserPresenterTest >> testRemoveAll [
	
	chooserPresenter addAllButton click.
	self assert: chooserPresenter list unfilteredItems isEmpty.
	self assert: chooserPresenter selectedList unfilteredItems isNotEmpty.
	chooserPresenter removeAllButton click.
	self assert: chooserPresenter list unfilteredItems isNotEmpty.
	self assert: chooserPresenter selectedList unfilteredItems isEmpty.
]

{ #category : #running }
SpChooserPresenterTest >> testRemoveItem [

	chooserPresenter addAllButton click.
	self assert: chooserPresenter list unfilteredItems isEmpty.
	self assert: chooserPresenter selectedList unfilteredItems isNotEmpty.
	chooserPresenter selectedList selectItem: chooserPresenter selectedList unfilteredItems first.
	chooserPresenter removeButton click.
	self assert: chooserPresenter list unfilteredItems size equals: 1.
]

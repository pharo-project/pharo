"
A LtMenuPresenterBuilderTest is a test class for testing the behavior of LtMenuPresenterBuilder
"
Class {
	#name : #SpMenuPresenterBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'menuBuilder',
		'commandsGroup'
	],
	#category : #'Spec2-Commander2-Tests'
}

{ #category : #running }
SpMenuPresenterBuilderTest >> setUp [
	super setUp.
	menuBuilder := SpMenuPresenterBuilder new.
	commandsGroup := (CmCommandGroup named: 'root') asSpecGroup beRoot
		register:
			(CmBlockCommand new
				name: 'c1';
				block: [  ];
				yourself) asSpecCommand;
		register:
			((CmCommandGroup named: 'sub1') asSpecGroup
				register:
					(CmBlockCommand new
						name: 'c2';
						block: [  ];
						yourself) asSpecCommand;
				register:
					(CmBlockCommand new
						name: 'c3';
						block: [  ];
						yourself) asSpecCommand);
		register:
			(CmBlockCommand new
				name: 'c4';
				block: [  ];
				yourself) asSpecCommand;
		yourself
]

{ #category : #test }
SpMenuPresenterBuilderTest >> testVisitCommand [
	| menuItem evaluationResult |
	evaluationResult := nil.
	self assert: menuBuilder menuPresenter defaultGroup menuItems isEmpty.
	(CmBlockCommand new
		name: 'c';
		description: 'desc';
		context: [ 41 ];
		block: [ :x | evaluationResult := x + 1 ];
		yourself) asSpecCommand acceptVisitor: menuBuilder.
	self
		assert: menuBuilder menuPresenter defaultGroup menuItems size
		equals: 1.
	menuItem := menuBuilder menuPresenter defaultGroup menuItems first.
	self assert: menuItem class equals: SpMenuItemPresenter.
	self assert: menuItem name equals: 'c'.
	self assert: menuItem description equals: 'desc'.
	self assert: evaluationResult isNil.
	menuItem action value.
	self assert: evaluationResult equals: 42
]

{ #category : #test }
SpMenuPresenterBuilderTest >> testVisitCommandThatCantBeRunWithDisableStrategy [
	self assert: menuBuilder menuPresenter defaultGroup menuItems isEmpty.
	(CmBlockCommand new
		name: 'c';
		description: 'desc';
		context: [ 41 ];
		canBeExecutedBlock: [ :x | x = 42 ];
		block: [ :x | 42 ];
		yourself) asSpecCommand acceptVisitor: menuBuilder.
	self
		assert: menuBuilder menuPresenter defaultGroup menuItems size
		equals: 1
]

{ #category : #test }
SpMenuPresenterBuilderTest >> testVisitCommandThatCantBeRunWithHideStrategy [
	self assert: menuBuilder menuPresenter defaultGroup menuItems isEmpty.
	(CmBlockCommand new
		name: 'c';
		description: 'desc';
		context: [ 41 ];
		canBeExecutedBlock: [ :x | x = 42 ];
		block: [ :x | 42 ];
		yourself) asSpecCommand beHiddenWhenCantBeRun
		acceptVisitor: menuBuilder.
	self assert: menuBuilder menuPresenter defaultGroup menuItems isEmpty
]

{ #category : #test }
SpMenuPresenterBuilderTest >> testVisitCommandsGroup [
	| menuItems |
	self assert: menuBuilder menuPresenter defaultGroup menuItems isEmpty.
	
	commandsGroup acceptVisitor: menuBuilder.
	
	menuItems := menuBuilder menuPresenter defaultGroup menuItems.
	self assert: menuItems size equals: 3.
	self assert: menuItems first class equals: SpMenuItemPresenter.
	self assert: menuItems first name equals: commandsGroup entries first name.
	self assert: menuItems first description equals: commandsGroup entries first description.
	self assert: menuItems first subMenu isNil.
	self assert: menuItems second class equals: SpMenuItemPresenter.
	self assert: menuItems second name equals: commandsGroup entries second name.
	self assert: menuItems second description equals: commandsGroup entries second description.
	self assert: menuItems second subMenu isNotNil.
	self assert: menuItems second subMenu class equals: SpMenuPresenter.
	self assert: menuItems second subMenu menuGroups size equals: 1.
	self assert: menuItems second subMenu menuGroups first menuItems size equals: 2.
	self assert: menuItems second subMenu menuGroups first menuItems first class equals: SpMenuItemPresenter.
	self assert: menuItems second subMenu menuGroups first menuItems first name equals: commandsGroup entries second entries first name.
	self assert: menuItems second subMenu menuGroups first menuItems first description equals: commandsGroup entries second entries first description.
	self assert: menuItems second subMenu menuGroups first menuItems second class equals: SpMenuItemPresenter.
	self assert: menuItems second subMenu menuGroups first menuItems second name equals: commandsGroup entries second entries second name.
	self assert: menuItems second subMenu menuGroups first menuItems second description equals: commandsGroup entries second entries second description.
	self assert: menuItems third class equals: SpMenuItemPresenter.
	self assert: menuItems third name equals: commandsGroup entries third name.
	self assert: menuItems third description equals: commandsGroup entries third description.
	self assert: menuItems third subMenu isNil.
]

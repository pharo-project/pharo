"
I open, as a pop-up menu, the commands of the context menu that are declared in one menu group, such as the refactorings of a method. I let a shortcut or a tab action open a sub-menu of the context menu on its own:
	aCommandMap shortcuts addMenuOf: SycMethodRefactoringMenuGroup key: $t meta.
	aCommandMap tabActions addMenuOf: SycClassRefactoringMenuGroup
My name, description and icon are the ones of my menu group, which can also prepare the context before the menu is built (ClyMenuGroup>>#prepareToOpenIn:).
When the sub-menu of my group is shown in a context menu, the keys of my shortcut are shown next to it.
"
Class {
	#name : 'ClyOpenMenuGroupCommand',
	#superclass : 'ClyCommand',
	#instVars : [
		'menuGroupClass'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'execution' }
ClyOpenMenuGroupCommand >> createMenu [
	"The pop-up menu of the commands of my group in the context menu of my context."

	| root group |
	root := ClyCommandGroupBuilder new buildMenuOf: (ClyCommandRegistry default activationsOf: #contextMenu inContext: context) context: context pureRoots: #(  ).
	group := root presentedGroupOf: menuGroupClass.
	^ ClyCommandGroupMenuRenderer new renderContentsOf: group on: context tool
]

{ #category : 'accessing' }
ClyOpenMenuGroupCommand >> description [

	^ menuGroupClass new description
]

{ #category : 'execution' }
ClyOpenMenuGroupCommand >> execute [

	| menu |
	menuGroupClass new prepareToOpenIn: context.
	menu := self createMenu.
	menu hasItems ifFalse: [ ^ self ].
	menu setInvokingView: context tool.
	menu invokeModal
]

{ #category : 'accessing' }
ClyOpenMenuGroupCommand >> iconName [

	^ menuGroupClass new iconName
]

{ #category : 'accessing' }
ClyOpenMenuGroupCommand >> menuGroupClass [

	^ menuGroupClass
]

{ #category : 'accessing' }
ClyOpenMenuGroupCommand >> menuGroupClass: aMenuGroupClass [

	menuGroupClass := aMenuGroupClass
]

{ #category : 'accessing' }
ClyOpenMenuGroupCommand >> name [

	^ menuGroupClass new name
]

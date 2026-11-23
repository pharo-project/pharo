"
I am the command which opens the menu of the left bar of a text editor: the commands declared for the #textLeftBarMenu interaction of ClyCommandMap. When no text is selected, I first show the source node of the method, and then I open the menu.
"
Class {
	#name : 'ClyOpenTextLeftBarMenuCommand',
	#superclass : 'ClyCommand',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'execution' }
ClyOpenTextLeftBarMenuCommand >> createContextMenu [

	^ ClyCommandGroupMenuRenderer new
		  renderTree: (ClyCommandGroupBuilder new buildMenuOf: (ClyCommandRegistry default activationsOf: #textLeftBarMenu inContext: self context) context: self context pureRoots: #(  ))
		  on: self context tool
]

{ #category : 'execution' }
ClyOpenTextLeftBarMenuCommand >> execute [

	| menu |
	self context selectedTextInterval ifEmpty: [ self context showSourceNode ].
	menu := self createContextMenu.
	menu hasItems ifFalse: [ ^ self ].
	menu setInvokingView: self context tool.
	menu invokeModal
]

{ #category : 'accessing' }
ClyOpenTextLeftBarMenuCommand >> name [

	^ 'Open self context menu'
]

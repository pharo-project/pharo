Extension { #name : #CmdMenu }

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenu >> buildContextMenuFor: aMorph [

	| menu |
	menu := UIManager default newMenuIn: aMorph for: aMorph.

	rootGroup buildContextMenu: menu.
	
	menu hasSubmorphs ifTrue: [ 
		menu lastSubmorph isMenuLineMorph ifTrue: [ menu lastSubmorph delete ]].
	^menu
]

{ #category : #'*Commander-Activators-ContextMenu' }
CmdMenu >> registerContextMenuItemsWithBuilder: aBuilder [

	rootGroup registerContextMenuItemsWithBuilder: aBuilder
]

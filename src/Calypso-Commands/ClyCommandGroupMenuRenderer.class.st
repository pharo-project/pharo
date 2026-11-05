"
I render a Commander2 command group (CmCommandGroup) into a Morphic menu (MenuMorph).

Each command of the group becomes a menu item using its name, description, icon and
canBeExecuted status. I am the Morphic counterpart of the Spec2-Commander2 renderer
(#asSpecGroup) and exist so Calypso can display Commander2 commands in its Morphic
context menus.
"
Class {
	#name : 'ClyCommandGroupMenuRenderer',
	#superclass : 'Object',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyCommandGroupMenuRenderer class >> render: aCommandGroup on: aMenuMorph [

	^ self new
		  render: aCommandGroup
		  on: aMenuMorph
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> render: aCommandGroup on: aMenuMorph [

	aCommandGroup commands do: [ :command | self renderCommand: command on: aMenuMorph ].
	^ aMenuMorph
]

{ #category : 'rendering' }
ClyCommandGroupMenuRenderer >> renderCommand: aCommand on: aMenuMorph [

	| item |
	item := aMenuMorph
		        add: aCommand name
		        target: aCommand
		        selector: #execute.
	item enabled: aCommand canBeExecuted.
	item setBalloonText: aCommand description.
	"To clean."
	(aCommand respondsTo: #icon) ifTrue: [
		aCommand icon ifNotNil: [ :anIcon | item icon: anIcon ] ].
	^ item
]

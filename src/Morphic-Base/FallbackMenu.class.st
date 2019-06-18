"
I appear when there is an error while trying to build a menu from pragmas.

I include items to:
* Debug: give the user an opportunity to see and correct the error
* Explain: why they're seeing a different menu
* Custom: clients can add items to me by implementing #fallbackMenuOn:. Browse implementors for examples.
"
Class {
	#name : #FallbackMenu,
	#superclass : #Object,
	#instVars : [
		'builder',
		'client'
	],
	#category : #'Morphic-Base-Menus'
}

{ #category : #'instance creation' }
FallbackMenu class >> when: aPragmaMenuBuilder fails: anObject [
	
	^ self new
			builder: aPragmaMenuBuilder;
			client: anObject;
			menu.
]

{ #category : #private }
FallbackMenu >> builder: aPragmaMenuBuilder [

	builder := aPragmaMenuBuilder.
]

{ #category : #private }
FallbackMenu >> client: anObject [

	client := anObject.
]

{ #category : #'menu actions' }
FallbackMenu >> debug [

	"If we don't reset the builder after debugging, the fallback menu does not appear anymore"
	[ builder menuSpec asMenuMorph ] ensure: [ builder reset ].
]

{ #category : #private }
FallbackMenu >> fallbackMenuExplanations [

	| workspace wsBindings |
	wsBindings := Dictionary newFrom: { #menu-> self }.
	workspace := Smalltalk tools workspace
		openContents: '"Something is wrong with this menu. 
To investigate just debug following expression:"
menu debug'.
	workspace
		label: 'Fallback menu explanations';
		setBindings: wsBindings.
]

{ #category : #private }
FallbackMenu >> menu [
	"Build the menu that is put up if something is going wrong with the menubuilder"

	| menu |
	menu := UIManager default newMenuIn: self currentWorld for: self.
	menu buildTitle: [ :menuTitle | menuTitle
		onlyCloseAndPinable
	].
	menu add: 'Why you see this menu'
		target: self
		selector: #fallbackMenuExplanations.
	menu add: 'Debug'
		target: self
		selector: #debug.
	menu addLine.
	client fallbackMenuOn: menu.
	^ menu
]

"
A presenter to describe a menuBar. 
I'm different from a simple menu in: 

- I cannot be used as submenu (simple menus can).
- I will render a menuBar instead a simple menu (this can be the same as a menu in certain platforms, but others may differ).

Tipically, this menu will be presented at the top of a window.
"
Class {
	#name : #SpMenuBarPresenter,
	#superclass : #SpMenuPresenter,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpMenuBarPresenter class >> adapterName [

	^ #MenuBarAdapter
]

{ #category : #documentation }
SpMenuBarPresenter class >> addDocumentExtraSections: aBuilder [
	"It does not applies here"
]

{ #category : #documentation }
SpMenuBarPresenter class >> documentFactoryMethodSelector [

	^ #newMenuBar
]

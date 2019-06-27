"
I'm a presenter to describe a menuBar. 
I'm different from a simple menu in: 

- I cannot be used as submenu (simple menus can).
- I will render a menuBar instead a simple menu (this can be the same as a menu in certain 
  platforms, but others may differ).
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

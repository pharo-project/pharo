"
I'm a presenter to describe a menuBar. 
I'm different from a simple menu in: 

- I cannot be used as submenu (simple menus can).
- I will render a menuBar instead a simple menu (this can be the same as a menu in certain 
  platforms, but others may differ).
"
Class {
	#name : #MenuBarPresenter,
	#superclass : #MenuPresenter,
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
MenuBarPresenter class >> adapterName [

	^ #MenuBarAdapter
]

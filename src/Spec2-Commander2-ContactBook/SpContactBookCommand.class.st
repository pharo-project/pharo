"
I am an abstract command for the contact book example.

I define methods to access information in my #context more easily.
"
Class {
	#name : #SpContactBookCommand,
	#superclass : #CmCommand,
	#category : #'Spec2-Commander2-ContactBook-Commands'
}

{ #category : #accessing }
SpContactBookCommand >> contactBook [
	"Again, this is totally optional, it justs make access to selected contact easier.
	 Thus code in #execute is easier to read.
	"
	^ self contactBookPresenter contactBook
]

{ #category : #accessing }
SpContactBookCommand >> contactBookPresenter [
	"Optional, simply aliasing the context (the LtContactBookPresenter) to a more explicit name.
	 It will just make the code easier to read in #execute methods of my subclasses.
	"
	^ self context
]

{ #category : #testing }
SpContactBookCommand >> hasSelectedContact [
	^ self contactBookPresenter isContactSelected
]

{ #category : #accessing }
SpContactBookCommand >> selectedContact [
	"Again, this is totally optional, it justs make access to selected contact easier.
	 Thus code in #execute is easier to read.
	"
	^ self contactBookPresenter selectedContact
]

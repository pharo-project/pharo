"
i'm an AbstractClass. i'm an interface to describe NoteBookPage

my subclass define :
	* provider : the wiget(s) what you want to show 
		example: 
			* TextPresenter if you want to show comment of a class 
			* CodePresenter if you want to show a critics  
	* title : return the title of the bookPage
	
"
Class {
	#name : #CBAbstractNoteBookPage,
	#superclass : #Object,
	#category : #'Tool-CriticBrowser-utilities-tabContent'
}

{ #category : #accessing }
CBAbstractNoteBookPage class >> icon [
	"hook"
	^ nil
]

{ #category : #testing }
CBAbstractNoteBookPage class >> isAbstract [
	^ self = CBAbstractNoteBookPage
]

{ #category : #accessing }
CBAbstractNoteBookPage class >> layoutSpec [
	^ self subclassResponsibility
]

{ #category : #accessing }
CBAbstractNoteBookPage class >> provider: aCriticBrowser [
	^ self subclassResponsibility 
]

{ #category : #accessing }
CBAbstractNoteBookPage class >> title [
	^ self subclassResponsibility
]

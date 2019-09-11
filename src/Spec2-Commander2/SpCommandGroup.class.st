"
I am a command group decorator adding informations useful when for usage in context of a Spec application.

Basically, I add:
- an #icon (#blank by default)
- the strategy to display commands group in a MenuPresenter
"
Class {
	#name : #SpCommandGroup,
	#superclass : #CmCommandGroupDecorator,
	#instVars : [
		'displayStrategy',
		'isRoot',
		'icon'
	],
	#category : #'Spec2-Commander2-Core'
}

{ #category : #default }
SpCommandGroup class >> defaultDisplayStrategy [

	^ SpDisplayAsSubMenu new
]

{ #category : #default }
SpCommandGroup class >> defaultIconName [
	^ #blank
]

{ #category : #converting }
SpCommandGroup >> asMenuBarPresenter [
	^ SpMenuBarPresenterBuilder new
		visit: self;
		menuPresenter
]

{ #category : #converting }
SpCommandGroup >> asMenuBarPresenterWith: aBlock [
	| builder |

	builder := SpMenuBarPresenterBuilder new.
	aBlock value: builder menuPresenter.
	^ builder
		visit: self;
		menuPresenter
]

{ #category : #converting }
SpCommandGroup >> asMenuPresenter [
	^ SpMenuPresenterBuilder new
		visit: self;
		menuPresenter
]

{ #category : #converting }
SpCommandGroup >> asMenuPresenterWith: aBlock [
	| builder |

	builder := SpMenuPresenterBuilder new.
	aBlock value: builder menuPresenter.
	^ builder
		visit: self;
		menuPresenter
]

{ #category : #converting }
SpCommandGroup >> asToolBarPresenter [
	^ SpToolBarPresenterBuilder new
		visit: self;
		toolbarPresenter
]

{ #category : #converting }
SpCommandGroup >> asToolBarPresenterWith: aBlock [
	| builder |
	
	builder := SpToolBarPresenterBuilder new.
	aBlock value: builder toolbarPresenter.
	^ builder
		visit: self;
		toolbarPresenter
]

{ #category : #configuring }
SpCommandGroup >> beDisplayedAsGroup [
	self displayStrategy: SpDisplayAsGroup new
]

{ #category : #configuring }
SpCommandGroup >> beDisplayedAsSubMenu [
	self displayStrategy: SpDisplayAsSubMenu new
]

{ #category : #configuring }
SpCommandGroup >> beRoot [
	self isRoot: true.
]

{ #category : #displaying }
SpCommandGroup >> displayIn: aMenuPresenter do: aBlock [

	self displayStrategy 
		display: self 
		in: aMenuPresenter 
		do: aBlock
]

{ #category : #accessing }
SpCommandGroup >> displayStrategy [
	^ displayStrategy
]

{ #category : #accessing }
SpCommandGroup >> displayStrategy: anObject [
	displayStrategy := anObject
]

{ #category : #accessing }
SpCommandGroup >> icon [ 

	^ icon
]

{ #category : #accessing }
SpCommandGroup >> icon: anIcon [

	icon := anIcon
]

{ #category : #accessing }
SpCommandGroup >> iconName: aSymbol [


	self icon: (self iconNamed: aSymbol)
]

{ #category : #initialization }
SpCommandGroup >> initialize [

	super initialize.
	self displayStrategy: self class defaultDisplayStrategy.
	self isRoot: false
]

{ #category : #'shortcuts installation' }
SpCommandGroup >> installShortcutsIn: aPresenter [ 
	SpShortcutInstaller new
		presenter: aPresenter;
		visit: self
]

{ #category : #accessing }
SpCommandGroup >> isRoot [
	^ isRoot
]

{ #category : #accessing }
SpCommandGroup >> isRoot: anObject [
	isRoot := anObject
]

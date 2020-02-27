"
I am a command group decorator adding informations useful when for usage in context of a Spec application.

Basically, I add:
- an #icon (#blank by default)
- the strategy to display commands group in a MenuPresenter
"
Class {
	#name : #SpCommandGroup,
	#superclass : #CmUICommandGroup,
	#category : #'Spec2-Commander2-Core'
}

{ #category : #converting }
SpCommandGroup >> asKMCategory [

	^ SpKMCategoryBuilder new
		visit: self;
		kmCategory	
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
SpCommandGroup >> asToolbarPresenter [
	^ SpToolBarPresenterBuilder new
		visit: self;
		toolbarPresenter
]

{ #category : #converting }
SpCommandGroup >> asToolbarPresenterWith: aBlock [
	| builder |
	
	builder := SpToolBarPresenterBuilder new.
	aBlock value: builder toolbarPresenter.
	^ builder
		visit: self;
		toolbarPresenter
]

{ #category : #displaying }
SpCommandGroup >> displayIn: aMenuPresenter do: aBlock [

	self displayStrategy 
		display: self 
		in: aMenuPresenter 
		do: aBlock
]

{ #category : #'shortcuts installation' }
SpCommandGroup >> installShortcutsIn: aPresenter [ 
	SpShortcutInstaller new
		presenter: aPresenter;
		visit: self
]

Class {
	#name : #SpecApplicationTest,
	#superclass : #TestCase,
	#instVars : [
		'application'
	],
	#category : #'Spec-Tests-Core-Base'
}

{ #category : #running }
SpecApplicationTest >> setUp [
	
	super setUp.
	application := MockApplication new
]

{ #category : #running }
SpecApplicationTest >> tearDown [
	
	application close.
	super tearDown
]

{ #category : #tests }
SpecApplicationTest >> testAdapterBindings [

	self assert: application adapterBindings class equals: SpecStubAdapterBindings.
	application useBackend: #Morphic.
	self assert: application adapterBindings class equals: MorphicAdapterBindings
	
]

{ #category : #tests }
SpecApplicationTest >> testCloseDialogWindowRemovesItFromWindowCollection [
	| window |
	window := (application newPresenter: ButtonPresenter) openDialogWithSpec.
	window close.
	self deny: (application windows includes: window)
]

{ #category : #tests }
SpecApplicationTest >> testCloseWindowRemovesItFromWindowCollection [
	| window |
	window := (application newPresenter: ButtonPresenter) openWithSpec.
	window close.
	self deny: (application windows includes: window)
]

{ #category : #tests }
SpecApplicationTest >> testIsPresenter [

	self deny: application isPresenter 
]

{ #category : #tests }
SpecApplicationTest >> testNewPresenter [
	| presenter |

	presenter := application newPresenter: ButtonPresenter.
	self assert: presenter application equals: application
]

{ #category : #tests }
SpecApplicationTest >> testOpenDialogWindowAddsItToWindowCollection [
	| window |
	window := (application newPresenter: ButtonPresenter) openDialogWithSpec.
	
	self assert: (application windows includes: window)
]

{ #category : #tests }
SpecApplicationTest >> testOpenDialogWindowWithDifferentApplicationDoesNotAddItToWindowCollection [
	| secondApplication presenterInSecondApplication windowsBefore |

	secondApplication := SpecApplication new.
	presenterInSecondApplication := secondApplication newPresenter: ButtonPresenter.

	windowsBefore := application windows copy.

	self
		should: [ application openDialog: presenterInSecondApplication ]
		raise: SpecInvalidApplicationError.
		
	self assert: application windows equals: windowsBefore
]

{ #category : #tests }
SpecApplicationTest >> testOpenDialogWindowWithDifferentApplicationShouldFail [
	| secondApplication presenterInSecondApplication |

	secondApplication := SpecApplication new.
	presenterInSecondApplication := secondApplication newPresenter: ButtonPresenter.

	self
		should: [ application openDialog: presenterInSecondApplication ]
		raise: SpecInvalidApplicationError
]

{ #category : #tests }
SpecApplicationTest >> testOpenWindowAddsItToWindowCollection [
	| window |
	window := (application newPresenter: ButtonPresenter) openWithSpec.
	
	self assert: (application windows includes: window)
]

{ #category : #tests }
SpecApplicationTest >> testOpenWindowWithDifferentApplicationDoesNotAddItToWindowCollection [
	| secondApplication presenterInSecondApplication windowsBefore |

	secondApplication := SpecApplication new.
	presenterInSecondApplication := secondApplication newPresenter: ButtonPresenter.

	windowsBefore := application windows copy.

	self should: [
		application 
			open: presenterInSecondApplication
			withSpecLayout: ButtonPresenter defaultSpec ] raise: SpecInvalidApplicationError.
		
	self assert: application windows equals: windowsBefore
]

{ #category : #tests }
SpecApplicationTest >> testOpenWindowWithDifferentApplicationShouldFail [
	| secondApplication presenterInSecondApplication |

	secondApplication := SpecApplication new.
	presenterInSecondApplication := secondApplication newPresenter: ButtonPresenter.

	self
		should: [ application open: presenterInSecondApplication ]
		raise: SpecInvalidApplicationError
]

{ #category : #tests }
SpecApplicationTest >> testUseBackend [

	self assert: application backend name equals: #Mock.
	application useBackend: #Morphic.
	self assert: application backend name equals: #Morphic
]

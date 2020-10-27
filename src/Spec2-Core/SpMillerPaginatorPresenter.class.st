"
A presenter that chains a miller list (`SpMillerColumnPresenter`) along with a paginator (`SpPaginatorPresenter`). 
This is the most common usage of paginator, and there are several places where we want to use a miller list with pagination and this common presenter will be provifing basic functionality pre-made for consumption.
"
Class {
	#name : #SpMillerPaginatorPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'millerList',
		'paginator',
		'paginatorContainerLayout'
	],
	#category : #'Spec2-Core-Widgets-Advanced'
}

{ #category : #initialization }
SpMillerPaginatorPresenter >> initializePresenters [

	self layout: (SpBoxLayout newVertical
		spacing: 3;
		add: (millerList := self instantiate: SpMillerColumnPresenter);
		add: ((paginatorContainerLayout := SpBoxLayout newHorizontal)
				hAlignCenter; 
				yourself)
			expand: false;
		yourself).
		
	millerList withoutHorizontalScrollBar.
	millerList whenColumnsChangedDo: [ :page | self updatePaginator: page ].
	self updatePaginator: 1
]

{ #category : #api }
SpMillerPaginatorPresenter >> millerListPresenter [

	^ millerList
]

{ #category : #private }
SpMillerPaginatorPresenter >> newPaginator [
		
	^ self instantiate: SpPaginatorPresenter
]

{ #category : #api }
SpMillerPaginatorPresenter >> pages [

	^ millerList pages
]

{ #category : #api }
SpMillerPaginatorPresenter >> paginatorPresenter [

	^ paginator
]

{ #category : #private }
SpMillerPaginatorPresenter >> updatePaginator: pageSelected [

	millerList pages size > 1 
		ifTrue: [ 
			"add a paginator, chained to the miller list"
			paginator ifNil: [ 
				paginator := self newPaginator.
				paginatorContainerLayout add: paginator expand: false.
				paginator chainTo: millerList ].
			paginator pages: millerList pages.
			paginator selectPage: pageSelected ]
		ifFalse: [
			"we do not want the paginator, let's remove it"
			paginatorContainerLayout removeAll. 
			paginator := nil ]
]

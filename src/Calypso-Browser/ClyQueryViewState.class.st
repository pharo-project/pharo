"
I represent the state of particular query view.
I keep data source and current selection of view.

I am supposed to not keep reference to the result of data source query.
So when I am initialized I ask data source for clean copy: 

	aQueryView dataSource copyForBrowserStateSnapshot.

And I convert current view selection to desired selection and clean it by detaching items from active data source.
Look at method #initializeFrom: for details.

My instances are created from query views: 

	ClyQueryViewState of: aQueryView

Or simply ask 
	
	aQueryView snapshotState
	
Internal Representation and Key Implementation Points.

    Instance Variables
	dataSource:		<ClyDataSource>
	selection:		<ClyDesiredDataSource>
"
Class {
	#name : #ClyQueryViewState,
	#superclass : #Object,
	#instVars : [
		'dataSource',
		'selection'
	],
	#category : #'Calypso-Browser-NavigationHistory'
}

{ #category : #operations }
ClyQueryViewState >> applyTo: aQueryView [

	| newDataSource |
	(aQueryView dataSource isInSameStateAs: dataSource) ifFalse: [ 
		newDataSource := dataSource copyForBrowserStateSnapshot.
		aQueryView dataSource: newDataSource].
	
	selection restoreCurrentSelection: aQueryView selection.
	aQueryView selection ensureVisibleLastItem
]

{ #category : #accessing }
ClyQueryViewState >> dataSource [
	^ dataSource
]

{ #category : #accessing }
ClyQueryViewState >> dataSource: anObject [
	dataSource := anObject
]

{ #category : #testing }
ClyQueryViewState >> isCurrentStateOf: aQueryView [
	
	^(aQueryView dataSource isInSameStateAs: dataSource)
		and: [aQueryView selection isSameAs: selection]
]

{ #category : #testing }
ClyQueryViewState >> representsConcreteQuery [
	"this method allows to detect that query view is in state when it not shows any concrete query. It is happens when view was not requested to show anything yet. In that case data source is initialized with unknown query instance"
	
	^(dataSource isBasedOn: ClyUnknownQuery instance) not
]

{ #category : #accessing }
ClyQueryViewState >> selection [
	^ selection
]

{ #category : #accessing }
ClyQueryViewState >> selection: anObject [
	selection := anObject
]

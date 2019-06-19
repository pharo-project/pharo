"
I represent the item of project.
Concrete project manager plugins are supposed to implement my subclasses to represent concrete types of project items.

Subclasses should implement following methods:
- items
- allPackages
- classes
- isEmpty

Instancees should be creation with project: 

	ClyProjectChildItem project: aProject
	 
Internal Representation and Key Implementation Points.

    Instance Variables
	project:		<ClyManagedProject>
"
Class {
	#name : #ClyProjectChildItem,
	#superclass : #Object,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'item convertation' }
ClyProjectChildItem class >> convertToCalypsoBrowserItem: aProjectChildItem [ 
	| item |
	item := ClyBrowserItem named: aProjectChildItem name with: aProjectChildItem.
	aProjectChildItem isEmpty ifFalse: [ item markWithChildrenOf: ClyProjectChildItem].
	^item
]

{ #category : #sorting }
ClyProjectChildItem class >> defaultSortFunctionForCalypso [ 
	^ClySortByNameFunction ascending
]

{ #category : #queries }
ClyProjectChildItem class >> prepareClassQueriesFrom: projectItems in: aNavigationEnvironment [ 
	^{ClyAllClassesQuery from: ClyProjectScope ofAll: projectItems in: aNavigationEnvironment }
]

{ #category : #queries }
ClyProjectChildItem class >> prepareProjectItemsQueryFrom: aProjectChildItem in: aNavigationEnvironment [ 

	^ ClyProjectContentQuery 
			sortedFrom: (ClyProjectScope of: aProjectChildItem in: aNavigationEnvironment)
]

{ #category : #'instance creation' }
ClyProjectChildItem class >> project: aProject [
	^self new 
		project: aProject
]

{ #category : #testing }
ClyProjectChildItem >> isEmpty [
	self subclassResponsibility 
]

{ #category : #accessing }
ClyProjectChildItem >> items [
	self subclassResponsibility 
	
]

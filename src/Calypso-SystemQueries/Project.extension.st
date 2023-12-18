Extension { #name : 'Project' }

{ #category : '*Calypso-SystemQueries' }
Project classSide >> convertToCalypsoBrowserItem: aProject [

	| item |
	item := ClyBrowserItem named: aProject name with: aProject.
	"We cannot use `self` because my subclass also need to return Project.."
	aProject hasPackages ifTrue: [ item markWithChildrenOf: Project ].
	^ item
]

{ #category : '*Calypso-SystemQueries' }
Project classSide >> defaultSortFunctionForCalypso [

	^ ClySortByNameFunction ascending
]

{ #category : '*Calypso-SystemQueries' }
Project class >> prepareClassQueriesFrom: packages in: aNavigationEnvironment [

	^ { (ClyAllClassesQuery from: ClyProjectScope ofAll: packages in: aNavigationEnvironment) }
]

{ #category : '*Calypso-SystemQueries' }
Project classSide >> prepareProjectItemsQueryFrom: aProjectChildItem in: aNavigationEnvironment [

	^ ClyProjectContentQuery sortedFrom: (ClyProjectScope of: aProjectChildItem in: aNavigationEnvironment)
]

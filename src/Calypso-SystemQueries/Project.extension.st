Extension { #name : 'Project' }

{ #category : '*Calypso-SystemQueries' }
Project class >> convertToCalypsoBrowserItem: aProject [

	| item |
	item := ClyBrowserItem named: aProject name with: aProject.
	"We cannot use `self` because my subclass also need to return Project.."
	aProject hasPackages ifTrue: [ item markWithChildrenOf: Project ].
	^ item
]

{ #category : '*Calypso-SystemQueries' }
Project class >> defaultSortFunctionForCalypso [

	^ ClySortByNameFunction ascending
]

{ #category : '*Calypso-SystemQueries' }
Project >> includesClassesAffectedBy: aSystemAnnouncement [

	^ self packages anySatisfy: [ :package | package includesClassesAffectedBy: aSystemAnnouncement ]
]

{ #category : '*Calypso-SystemQueries' }
Project class >> prepareClassQueriesFrom: projects in: aNavigationEnvironment [

	^ {  }
]

{ #category : '*Calypso-SystemQueries' }
Project class >> prepareProjectItemsQueryFrom: aProjectChildItem in: aNavigationEnvironment [

	^ ClyProjectContentQuery sortedFrom: (ClyProjectScope of: aProjectChildItem in: aNavigationEnvironment)
]

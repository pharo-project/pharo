Extension { #name : #AbstractPackage }

{ #category : #'*Calypso-SystemQueries' }
AbstractPackage class >> convertToCalypsoBrowserItem: aPackage [

	^ClyBrowserItem named: aPackage name with: aPackage
]

{ #category : #'*Calypso-SystemQueries' }
AbstractPackage class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	^ anEnvironmentPlugin
		decorateBrowserItem: aBrowserItem
		ofPackage: aBrowserItem actualObject
]

{ #category : #'*Calypso-SystemQueries' }
AbstractPackage class >> defaultSortFunctionForCalypso [
	^ClySortByNameFunction ascending
]

{ #category : #'*Calypso-SystemQueries' }
AbstractPackage >> definesOverridesOf: aMethod [

	| methodClass selector |
	methodClass := aMethod origin.
	selector := aMethod selector.

	self classes
		select: [ :class | class inheritsFrom: methodClass ]
		thenDo: [ :class | ((self selectorsForClass: class) includes: selector) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #'*Calypso-SystemQueries' }
AbstractPackage >> importClasses: classCollection [
	classCollection do: [ :each | self addClass: each ]
]

{ #category : #'*Calypso-SystemQueries' }
AbstractPackage class >> itemsSortOrderForCalypso [
	^10
]

{ #category : #'*Calypso-SystemQueries' }
AbstractPackage class >> prepareClassGroupQueryFrom: aPackage in: aNavigationEnvironment [
	^ ClyAllClassGroupsQuery
		sortedFrom: (ClyPackageScope of: aPackage in: aNavigationEnvironment)
]

{ #category : #'*Calypso-SystemQueries' }
AbstractPackage class >> prepareClassQueriesFrom: packages in: aNavigationEnvironment [
	^{
		ClyAllClassesQuery from: ClyPackageScope ofAll: packages in: aNavigationEnvironment.
		ClyAllClassesQuery from: ClyPackageExtensionScope ofAll: packages in: aNavigationEnvironment
	}
]

{ #category : #'*Calypso-SystemQueries' }
AbstractPackage >> tagsForClasses [
	"Any class could be tagged for user purpose.
	Now we implement it on top of RPackageTag"

	^self classTags reject: [:each | each isRoot] thenCollect: [:each | each name]
]

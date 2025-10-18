Extension { #name : 'Package' }

{ #category : '*Calypso-SystemQueries' }
Package class >> convertToCalypsoBrowserItem: aPackage [

	^ClyBrowserItem named: aPackage name with: aPackage
]

{ #category : '*Calypso-SystemQueries' }
Package class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	^ anEnvironmentPlugin
		decorateBrowserItem: aBrowserItem
		ofPackage: aBrowserItem actualObject
]

{ #category : '*Calypso-SystemQueries' }
Package class >> defaultSortFunctionForCalypso [
	^ClySortByNameFunction ascending
]

{ #category : '*Calypso-SystemQueries' }
Package >> definesOverridesOf: aMethod [

	| methodClass selector |
	methodClass := aMethod origin.
	selector := aMethod selector.

	self definedClasses do: [ :class | (class inheritsFrom: methodClass) ifTrue: [ class definedSelectors includes: selector ] ].
	extensionSelectors keysAndValuesDo: [ :class :selectors | ((selectors includes: selector) and: [ class inheritsFrom: methodClass ]) ifTrue: [ ^ true ] ].

	^ false
]

{ #category : '*Calypso-SystemQueries' }
Package >> includesClassesAffectedBy: aSystemAnnouncement [

	^(aSystemAnnouncement affectsClassesDefinedInPackage: self)
		or: [ aSystemAnnouncement affectsClassesExtendedInPackage: self ]
]

{ #category : '*Calypso-SystemQueries' }
Package >> includesMethodsAffectedBy: aSystemAnnouncement [

	^aSystemAnnouncement affectsMethodsDefinedInPackage: self
]

{ #category : '*Calypso-SystemQueries' }
Package >> isAboutPackage: aPackage [

	^self == aPackage
]

{ #category : '*Calypso-SystemQueries' }
Package class >> itemsSortOrderForCalypso [
	^10
]

{ #category : '*Calypso-SystemQueries' }
Package class >> prepareClassGroupQueryFrom: aPackage in: aNavigationEnvironment [

	^ ClyAllPackageTagsQuery sortedFrom: (ClyPackageScope of: aPackage in: aNavigationEnvironment)
]

{ #category : '*Calypso-SystemQueries' }
Package class >> prepareClassQueriesFrom: packages in: aNavigationEnvironment [
	^{
		ClyAllClassesQuery from: ClyPackageScope ofAll: packages in: aNavigationEnvironment.
		ClyAllClassesQuery from: ClyPackageExtensionScope ofAll: packages in: aNavigationEnvironment
	}
]

{ #category : '*Calypso-SystemQueries' }
Package >> tagsForClasses [
	"Any class could be tagged for user purpose.
	Now we implement it on top of PackageTag"

	^ self tags
		  reject: [ :each | each isRoot ]
		  thenCollect: [ :each | each name ]
]

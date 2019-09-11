Extension { #name : #RPackage }

{ #category : #'*Calypso-SystemQueries' }
RPackage class >> convertToCalypsoBrowserItem: aPackage [

	^ClyBrowserItem named: aPackage name with: aPackage
]

{ #category : #'*Calypso-SystemQueries' }
RPackage class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	^ anEnvironmentPlugin
		decorateBrowserItem: aBrowserItem
		ofPackage: aBrowserItem actualObject
]

{ #category : #'*Calypso-SystemQueries' }
RPackage class >> defaultSortFunctionForCalypso [
	^ClySortByNameFunction ascending
]

{ #category : #'*Calypso-SystemQueries' }
RPackage >> definesOverridesOf: aMethod [

	aMethod origin isInstanceSide 
		ifTrue: [  
			(self definesOverridesOf: aMethod in: classDefinedSelectors) ifTrue: [^true].
			(self definesOverridesOf: aMethod in: classExtensionSelectors) ifTrue: [^true]]
		ifFalse: [ 
			(self definesOverridesOfClassSide: aMethod in: metaclassDefinedSelectors) ifTrue: [^true].
			(self definesOverridesOfClassSide: aMethod in: metaclassExtensionSelectors) ifTrue: [^true]].
	
	^false
]

{ #category : #'*Calypso-SystemQueries' }
RPackage >> definesOverridesOf: aMethod in: classAndSelectors [

	| methodClass selector |
	methodClass := aMethod origin.
	selector := aMethod selector.
	
	classAndSelectors keysAndValuesDo: [ :className :selectors |
		((selectors includes: selector) 
			and: [ (Smalltalk globals classNamed: className) inheritsFrom: methodClass ])
				ifTrue: [ ^true ] ].
	^false
]

{ #category : #'*Calypso-SystemQueries' }
RPackage >> definesOverridesOfClassSide: aMethod in: classAndSelectors [

	| methodClass selector |
	methodClass := aMethod origin.
	selector := aMethod selector.
	
	classAndSelectors keysAndValuesDo: [ :className :selectors |
		((selectors includes: selector) 
			and: [ (Smalltalk globals classNamed: className) classSide inheritsFrom: methodClass ])
				ifTrue: [ ^true ] ].
	^false
]

{ #category : #'*Calypso-SystemQueries' }
RPackage >> importClasses: classCollection [
	classCollection do: [ :each | self addClass: each ]
]

{ #category : #'*Calypso-SystemQueries' }
RPackage >> includesClassesAffectedBy: aSystemAnnouncement [

	^(aSystemAnnouncement affectsClassesDefinedInPackage: self)
		or: [ aSystemAnnouncement affectsClassesExtendedInPackage: self ]
]

{ #category : #'*Calypso-SystemQueries' }
RPackage >> includesMethodsAffectedBy: aSystemAnnouncement [

	^aSystemAnnouncement affectsMethodsDefinedInPackage: self
]

{ #category : #'*Calypso-SystemQueries' }
RPackage >> isAboutPackage: aPackage [

	^self == aPackage
]

{ #category : #'*Calypso-SystemQueries' }
RPackage class >> itemsSortOrderForCalypso [ 
	^10
]

{ #category : #'*Calypso-SystemQueries' }
RPackage class >> prepareClassGroupQueryFrom: aPackage in: aNavigationEnvironment [
	^ ClyAllClassGroupsQuery
		sortedFrom: (ClyPackageScope of: aPackage in: aNavigationEnvironment)
]

{ #category : #'*Calypso-SystemQueries' }
RPackage class >> prepareClassQueriesFrom: packages in: aNavigationEnvironment [
	^{
		ClyAllClassesQuery from: ClyPackageScope ofAll: packages in: aNavigationEnvironment.
		ClyAllClassesQuery from: ClyPackageExtensionScope ofAll: packages in: aNavigationEnvironment 
	}
]

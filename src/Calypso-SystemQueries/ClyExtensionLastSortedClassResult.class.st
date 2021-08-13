"
I am a special class query result which put extension classes to the end of list.
I am used in class view to show all extensions in the bottom. 

To detect which classes are extensions I use the scope from where the query was executed. In fact I detect which classes are defined in building query scope. And the rest are extensions.
For this logic I find in scope the package which defines given class. So I expect that query scope understands #packagesDo:. Now it is ClyPackageScope and ClyProjectScope.
Look at #isQueryScopeDefinesClass: for details.

And at last step I format classes from each part. I use #baseQueryResult for this.
I can be configured to show classes in simple sorted way: 

	ClyExtensionLastSortedClasses simple 
	
or to show classes in sorted hierarchy: 

	ClyExtensionLastSortedClasses hierarchical
	
For other cases you can instantiate me by #using: method: 

	ClyExtensionLastSortedClasses using: ClySubclassHierarchy inverse asQueryResult
 
In addition I mark all extended classes with ClyExtendedClassTag which is used by UI to work with such classes differently.

Also notice that I am a kind of ClyBrowserQueryResult. So I convert all given classes to the ClyBrowserItem instances.

Internal Representation and Key Implementation Points.

    Instance Variables
	baseQueryResult:		<ClyQueryResult>
"
Class {
	#name : #ClyExtensionLastSortedClassResult,
	#superclass : #ClyBrowserQueryResult,
	#instVars : [
		'baseQueryResult'
	],
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #'instance creation' }
ClyExtensionLastSortedClassResult class >> hierarchical [
	^self using: ClyHierarchicallySortedClassesResult new
]

{ #category : #'instance creation' }
ClyExtensionLastSortedClassResult class >> simple [
	^self using: (ClySortedQueryResult using: ClySortByNameFunction ascending)
]

{ #category : #'instance creation' }
ClyExtensionLastSortedClassResult class >> using: aQueryResult [
	^self new
		baseQueryResult: aQueryResult
]

{ #category : #comparing }
ClyExtensionLastSortedClassResult >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ baseQueryResult = anObject baseQueryResult
]

{ #category : #accessing }
ClyExtensionLastSortedClassResult >> baseQueryResult [
	^ baseQueryResult
]

{ #category : #accessing }
ClyExtensionLastSortedClassResult >> baseQueryResult: anObject [
	baseQueryResult := anObject
]

{ #category : #building }
ClyExtensionLastSortedClassResult >> buildExtensionItemsFrom: classes [

	| extensions |
	extensions := self buildItemsFrom: classes.
	extensions do: [ :each | each markWith: ClyExtendedClassTag ].
	^extensions
]

{ #category : #building }
ClyExtensionLastSortedClassResult >> buildItemsFrom: classes [

	| subResult |
	subResult := baseQueryResult prepareNewFor: buildingQuery in: environment.
	subResult buildWith: classes.
	^subResult items collect: [:each | each asCalypsoBrowserItem ]
]

{ #category : #building }
ClyExtensionLastSortedClassResult >> fillWith: classes [

	| definedClasses extensions |
	definedClasses := IdentitySet withAll: classes.
	extensions := OrderedCollection new.
	classes do: [ :each | 
		(self isQueryScopeDefinesClass: each) ifFalse: [ 
			definedClasses remove: each.
			extensions add: each]].
	items := self buildItemsFrom: definedClasses.
	items addAll: (self buildExtensionItemsFrom: extensions)
]

{ #category : #comparing }
ClyExtensionLastSortedClassResult >> hash [
	^super hash bitXor: baseQueryResult hash
]

{ #category : #building }
ClyExtensionLastSortedClassResult >> isQueryScopeDefinesClass: aClass [

	| classPackage |
	classPackage := aClass package.
	buildingQuery scope packagesDo: [ :each | 
		(each isAboutPackage: classPackage) ifTrue: [ ^true ]].
	^false
]

Class {
	#name : 'MetacelloMVSResolverTest',
	#superclass : 'MetacelloTestCase',
	#instVars : [
		'graph'
	],
	#category : 'Metacello-MVS-Tests',
	#package : 'Metacello-MVS-Tests'
}

{ #category : 'specs' }
MetacelloMVSResolverTest class >> specAt: aRepositoryString [
	"A project spec asking aRepositoryString (github://owner/name:version/path), for the project its repository names, with its default groups"

	| rest |
	rest := aRepositoryString allButFirst: (aRepositoryString indexOfSubCollection: '://') + 2.
	^ MetacelloMVSGraph new specFor: aRepositoryString project: ((rest substrings: '/') second copyUpTo: $:) loads: #(  )
]

{ #category : 'specs' }
MetacelloMVSResolverTest class >> specsAt: aCollectionOfRepositoryStrings [

	^ aCollectionOfRepositoryStrings collect: [ :each | self specAt: each ]
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> date: aString [

	^ DateAndTime fromString: aString , 'T12:00:00+00:00'
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> givenADependencyOfAVersionNotSelected [

	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' ) of: 'b'.
	self versions: #( 'v1.3.0' 'v1.4.0' ) of: 'd'.
	self versions: #( 'v1.0.0' ) of: 'f'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.0.0' 'd:v1.4.0' ).
	self version: 'v1.0.0' of: 'b' requires: #( 'd:v1.3.0' ).
	self version: 'v1.3.0' of: 'd' requires: #( 'f:v1.0.0' ).
	self version: 'v1.4.0' of: 'd' requires: #(  ).
	self version: 'v1.0.0' of: 'f' requires: #(  )
]

{ #category : 'helpers' }
MetacelloMVSResolverTest >> installed: aDescription project: aName [
	"What the image has installed: the project spec it was loaded from, by the description aDescription"
	^ graph specFor: aDescription project: aName loads: #(  )
]

{ #category : 'running' }
MetacelloMVSResolverTest >> project: aProjectName at: anOwnerNameAndRef [
	"A project spec asking the baseline aProjectName in github://anOwnerNameAndRef/src, like a fork of it"

	^ graph specFor: 'github://' , anOwnerNameAndRef , '/src' project: aProjectName loads: #(  )
]

{ #category : 'tests - replace' }
MetacelloMVSResolverTest >> replaceSetUp [
	"The root requires Bloc v3.0.0 and Album, which wants Bloc v3.1.0; the root replaces Bloc by a fork"

	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v3.0.0' 'v3.1.0' ) of: 'bloc'.
	self versions: #( 'v3.0.1-fix' 'v3.0.2' ) of: 'fork'.
	self versions: #( 'v1.0.0' ) of: 'album'.
	graph
		repository: (self repo: 'root') at: 'v1.0.0' requires: {
				(self project: 'Bloc' at: 'o/bloc:v3.0.0').
				(self project: 'Album' at: 'o/album:v1.0.0') };
		repository: (self repo: 'album') at: 'v1.0.0' requires: { (self project: 'Bloc' at: 'o/bloc:v3.1.0') }.
	self version: 'v3.0.1-fix' of: 'fork' requires: #(  ).
	graph repository: (self repo: 'root') at: 'v1.0.0' replaces: { ('Bloc' -> 'github://o/fork:v3.0.1-fix/src') } asDictionary
]

{ #category : 'running' }
MetacelloMVSResolverTest >> repo: aName [

	^ 'github://o/' , aName
]

{ #category : 'running' }
MetacelloMVSResolverTest >> resolve: aRootNameAndRef [

	^ (MetacelloMVSResolver on: graph) resolve: (self repo: aRootNameAndRef) , '/src' project: (aRootNameAndRef copyUpTo: $:)
]

{ #category : 'helpers' }
MetacelloMVSResolverTest >> resolveRoots: aCollectionOfNamesAndRefs [
	"Resolve from several roots, each name:ref, in order"

	^ (MetacelloMVSResolver on: graph) resolveRoots: (aCollectionOfNamesAndRefs collect: [ :each |
			   graph specFor: (self repo: each) , '/src' project: (each copyUpTo: $:) loads: #(  ) ])
]

{ #category : 'running' }
MetacelloMVSResolverTest >> selected: aName in: aBuildList [

	^ (aBuildList at: (self repo: aName)) version string
]

{ #category : 'running' }
MetacelloMVSResolverTest >> setUp [

	super setUp.
	graph := MetacelloMVSGraph new
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testABranchIsAnErrorThatShowsTheChain [

	| report error |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' ) of: 'b'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.0.0' ).
	self version: 'v1.0.0' of: 'b' requires: #( 'c:master' ).
	error := (report := self resolve: 'a:v1.0.0') errors detect: [ :each | each kind = #unversionedReference ] ifNone: [ nil ].
	self assert: error isNotNil.
	self assert: (report descriptionOf: error) equals: (String lf join:
			 #( 'github://o/c:master/src names no version. Required by:' '  github://o/a v1.0.0' '  github://o/b v1.0.0' ))
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testACanonicalGraphHasNoWarnings [

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.0.0' ) of: 'a'.
	self version: 'v1.0.0' of: 'root' requires: #( 'a:v1.0.0' ).
	self version: 'v1.0.0' of: 'a' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root'.
	self
		assert: report resolves;
		assert: report warnings isEmpty
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testACommitIsNotAVersion [
	"Like gtoolkit's RBFormatter, required by a commit sha"

	self versions: #( 'v1.0.0' ) of: 'root'.
	self version: 'v1.0.0' of: 'root' requires: #( 'formatter:b35377f2c5f536bd851c8e6d5f6bcabc9a2f09c5' ).
	self assert: ((self resolve: 'root:v1.0.0') errors anySatisfy: [ :each | each kind = #unversionedReference ])
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAFloatingVersionIsTheFirstOfItsSeriesWithAWarning [
	"Like XMLParser asking for OrderPreservingDictionary:v1.6.x, a tag moved to v1.6.1"

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.5.0' 'v1.6.0' 'v1.6.1' 'v1.6.x' ) of: 'dictionary'.
	self version: 'v1.0.0' of: 'root' requires: #( 'dictionary:v1.6.x' ).
	self version: 'v1.6.0' of: 'dictionary' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root'.
	self assert: report resolves.
	self
		assert: (report descriptionOf: report warnings first)
		equals:
			(String lf join: #( 'github://o/dictionary:v1.6.x/src names a floating version, taken as its first version v1.6.0. Required by:'
				    '  github://o/root v1.0.0' )).
	self assert: (self selected: 'dictionary' in: (self resolve: 'root:v1.0.0') buildList) equals: 'v1.6.0'
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAFloatingVersionLosesToAHigherMinimum [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.0.0' ) of: 'other'.
	self versions: #( 'v0.9.0' 'v1.0.0' 'v1.3.0' 'v2.0.0' ) of: 'compat'.
	self version: 'v1.0.0' of: 'root' requires: #( 'compat:v1.x.x' 'other:v1.0.0' ).
	self version: 'v1.0.0' of: 'other' requires: #( 'compat:v1.3.0' ).
	self version: 'v1.0.0' of: 'compat' requires: #(  ).
	self version: 'v1.3.0' of: 'compat' requires: #(  ).
	buildList := (self resolve: 'root:v1.0.0') buildList.
	self assert: (self selected: 'compat' in: buildList) equals: 'v1.3.0'
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAFloatingVersionTakesTheFirstExistingVersionOfItsSeries [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.5.0' 'v1.6.1' 'v1.6.2' ) of: 'dictionary'.
	self version: 'v1.0.0' of: 'root' requires: #( 'dictionary:v1.6.x' ).
	self version: 'v1.6.1' of: 'dictionary' requires: #(  ).
	buildList := (self resolve: 'root:v1.0.0') buildList.
	self assert: (self selected: 'dictionary' in: buildList) equals: 'v1.6.1'
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testALeadingZeroIsAcceptedWithAWarning [
	"Like RoassalExporters, tagged v1.02"

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.01' 'v1.02' ) of: 'exporters'.
	self version: 'v1.0.0' of: 'root' requires: #( 'exporters:v1.02' ).
	self version: 'v1.02' of: 'exporters' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root'.
	self assert: report resolves.
	self assert: report warnings size equals: 1.
	self
		assert: (report descriptionOf: report warnings first)
		equals: (String lf join: #( 'github://o/exporters:v1.02/src names a version that is not vX.Y.Z. Required by:'
				    '  github://o/root v1.0.0' )).
	self assert: (self selected: 'exporters' in: (self resolve: 'root:v1.0.0') buildList) equals: 'v1.02'
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testAMalformedVersionStringIsNotAVersion [
	"Like opentelemetry-pharo asking for MethodProxies:v.2.0"

	self versions: #( 'v1.0.0' ) of: 'root'.
	self version: 'v1.0.0' of: 'root' requires: #( 'proxies:v.2.0' ).
	self assert: ((self resolve: 'root:v1.0.0') errors anySatisfy: [ :each | each kind = #unversionedReference ])
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testAMinimumThatDoesNotExistTakesTheNextVersion [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.5.0' 'v1.6.1' 'v1.6.2' ) of: 'b'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.6.0' ).
	self version: 'v1.6.1' of: 'b' requires: #(  ).
	buildList := (self resolve: 'a:v1.0.0') buildList.
	self assert: (self selected: 'b' in: buildList) equals: 'v1.6.1'
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testAMinorUpgradeCanDragAMajorConflict [
	"Like Album v0.10.0 and Toplo v0.11.0: Album v0.11.0 is selected, but Album v0.10.0 is visited too, and it asks for Bloc v2 while Toplo asks for Bloc v3"

	| report error |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v0.10.0' 'v0.11.0' ) of: 'album'.
	self versions: #( 'v0.11.0' ) of: 'toplo'.
	self versions: #( 'v2.9.0' 'v3.0.0' ) of: 'bloc'.
	self version: 'v1.0.0' of: 'root' requires: #( 'album:v0.10.0' 'toplo:v0.11.0' ).
	self version: 'v0.10.0' of: 'album' requires: #( 'bloc:v2.9.0' ).
	self version: 'v0.11.0' of: 'album' requires: #( 'bloc:v3.0.0' ).
	self version: 'v0.11.0' of: 'toplo' requires: #( 'album:v0.11.0' 'bloc:v3.0.0' ).
	self version: 'v2.9.0' of: 'bloc' requires: #(  ).
	self version: 'v3.0.0' of: 'bloc' requires: #(  ).
	error := (report := self resolve: 'root:v1.0.0') errors detect: [ :each | each kind = #majorConflict ] ifNone: [ nil ].
	self assert: error isNotNil.
	self assert:
		((report descriptionOf: error) includesSubstring: (String lf join: #( '  github://o/album v0.10.0' '  github://o/bloc v2.9.0' ))).
	self assert: ((report descriptionOf: error) includesSubstring: 'github://o/bloc v3.0.0')
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testAMinorUpgradeWithoutAMajorChangeResolves [
	"The same shape, with both Albums asking for Bloc v3: Album v0.11.0 wins"

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v0.10.0' 'v0.11.0' ) of: 'album'.
	self versions: #( 'v0.11.0' ) of: 'toplo'.
	self versions: #( 'v3.0.0' 'v3.1.0' ) of: 'bloc'.
	self version: 'v1.0.0' of: 'root' requires: #( 'album:v0.10.0' 'toplo:v0.11.0' ).
	self version: 'v0.10.0' of: 'album' requires: #( 'bloc:v3.0.0' ).
	self version: 'v0.11.0' of: 'album' requires: #( 'bloc:v3.1.0' ).
	self version: 'v0.11.0' of: 'toplo' requires: #( 'album:v0.11.0' 'bloc:v3.0.0' ).
	self version: 'v3.0.0' of: 'bloc' requires: #(  ).
	self version: 'v3.1.0' of: 'bloc' requires: #(  ).
	buildList := (self resolve: 'root:v1.0.0') buildList.
	self
		assert: (self selected: 'album' in: buildList) equals: 'v0.11.0';
		assert: (self selected: 'bloc' in: buildList) equals: 'v3.1.0'
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testAMissingRefIsNotAVersion [
	"Like Roassal's github://pharo-contributions/OSSubprocess, with no ref at all"

	| report error |
	self versions: #( 'v1.0.0' ) of: 'root'.
	graph repository: (self repo: 'root') at: 'v1.0.0' requires: (MetacelloMVSResolverTest specsAt: #( 'github://o/subprocess/src' )).
	error := (report := self resolve: 'root:v1.0.0') errors detect: [ :each | each kind = #unversionedReference ] ifNone: [ nil ].
	self assert: (report descriptionOf: error) equals: (String lf join:
			 #( 'github://o/subprocess/src names no version. Required by:' '  github://o/root v1.0.0' ))
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testAMissingVersionIsAnError [

	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' 'v1.2.0' 'v2.0.0' ) of: 'b'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.3.0' ).
	self assert: ((self resolve: 'a:v1.0.0') errors anySatisfy: [ :each | each kind = #missingVersion ])
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testAProjectOnlyAVersionNotSelectedRequiresIsDropped [
	"D 1.3 is not selected, so f, which only D 1.3 requires, is not used"

	| buildList |
	self givenADependencyOfAVersionNotSelected.
	buildList := (self resolve: 'a:v1.0.0') buildList.
	self
		assert: (self selected: 'd' in: buildList) equals: 'v1.4.0';
		deny: (buildList includesKey: (self repo: 'f'))
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testAProjectStillUsedKeepsTheVersionAReplacedVersionAsked [
	"A 1.0 -> B 1.0 -> C 1.2 -> E 3.5
	                 -> D 2.1 -> C 1.3 -> E 3.2
	C 1.3 still uses E, so E stays, at 3.5: the cleanup drops projects, it never lowers a version"

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' ) of: 'b'.
	self versions: #( 'v1.2.0' 'v1.3.0' ) of: 'c'.
	self versions: #( 'v2.1.0' ) of: 'd'.
	self versions: #( 'v3.2.0' 'v3.5.0' ) of: 'e'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.0.0' ).
	self version: 'v1.0.0' of: 'b' requires: #( 'c:v1.2.0' 'd:v2.1.0' ).
	self version: 'v1.2.0' of: 'c' requires: #( 'e:v3.5.0' ).
	self version: 'v1.3.0' of: 'c' requires: #( 'e:v3.2.0' ).
	self version: 'v2.1.0' of: 'd' requires: #( 'c:v1.3.0' ).
	self version: 'v3.2.0' of: 'e' requires: #(  ).
	self version: 'v3.5.0' of: 'e' requires: #(  ).
	buildList := (self resolve: 'a:v1.0.0') buildList.
	self
		assert: (self selected: 'c' in: buildList) equals: 'v1.3.0';
		assert: (self selected: 'e' in: buildList) equals: 'v3.5.0'
]

{ #category : 'tests - replace' }
MetacelloMVSResolverTest >> testAReplaceOfTheRootPinsItsVersionWithWarnings [

	| report buildList |
	self replaceSetUp.
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') , '/src' project: 'root'.
	self assert: report errors isEmpty.
	buildList := (self resolve: 'root:v1.0.0') buildList.
	self
		assert: (self selected: 'fork' in: buildList) equals: 'v3.0.1-fix';
		deny: (buildList includesKey: (self repo: 'bloc'));
		assert: (report warnings anySatisfy: [ :each | each text includesSubstring: 'Bloc is replaced by' ]);
		assert: (report warnings anySatisfy: [ :each | each text includesSubstring: 'asks for more than its replacement' ])
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAReportHasErrorsAndWarnings [

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.2' ) of: 'plural'.
	self version: 'v1.0.0' of: 'root' requires: #( 'plural:v1.2' 'b:development' ).
	self version: 'v1.2' of: 'plural' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root'.
	self
		deny: report resolves;
		assert: report errors size equals: 1;
		assert: report warnings size equals: 1
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAReportHasTheBuildList [

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v3.0.0' 'v3.1.0' ) of: 'bloc'.
	self version: 'v1.0.0' of: 'root' requires: #( 'bloc:v3.0.0' ).
	self version: 'v3.0.0' of: 'bloc' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') , '/src' project: 'root'.
	self
		assert: report resolves;
		assert: (self selected: 'bloc' in: report buildList) equals: 'v3.0.0';
		assert: (report dependenciesOf: (self repo: 'root')) asArray equals: { (self repo: 'bloc') };
		assert: (report rootCandidates collect: [ :each | each repository ]) asArray equals: { (self repo: 'root') }
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testARootWithTwoRepositoriesIsAnError [
	"Metacello lets a spec declare several repositories; mvs needs exactly one"
	| spec resolution |
	spec := (MetacelloProjectSpec project: MetacelloProject new)
		        name: 'a';
		        className: 'BaselineOfa';
		        repository: (self repo: 'a:v1.0.0');
		        repository: 'github://fork/a:v1.0.0';
		        yourself.
	resolution := (MetacelloMVSResolver on: graph) resolveRoots: { spec }.
	self
		assert: (resolution errors collect: [ :each | each text ]) asArray
		equals: #( '''a'' declares 2 repositories instead of one' )
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testASelectedNonCanonicalVersionIsAWarning [
	"Asking for v1.2.7 in a repository tagged 1.2.7"

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( '1.2.7' ) of: 'charset'.
	self version: 'v1.0.0' of: 'root' requires: #( 'charset:v1.2.7' ).
	self version: '1.2.7' of: 'charset' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root'.
	self assert: report resolves.
	self assert: (report warnings first text beginsWith: 'github://o/charset 1.2.7 is selected, a version that is not vX.Y.Z')
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testATwoNumberVersionIsAMinimumAndKeepsItsName [
	"Like Famix asking for SingularizePluralize:v1.2, a repository tagged v1.1, v1.2, v1.3"

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.1' 'v1.2' 'v1.3' ) of: 'plural'.
	self version: 'v1.0.0' of: 'root' requires: #( 'plural:v1.2' ).
	self version: 'v1.2' of: 'plural' requires: #(  ).
	buildList := (self resolve: 'root:v1.0.0') buildList.
	self assert: (self selected: 'plural' in: buildList) equals: 'v1.2'
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAWarningOfASelectedVersionIsAFinding [
	"The same floating version asked by the selected Album stays a warning"

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v0.11.0' ) of: 'album'.
	self versions: #( 'v3.0.0' ) of: 'bloc'.
	self version: 'v1.0.0' of: 'root' requires: #( 'album:v0.11.0' ).
	self version: 'v0.11.0' of: 'album' requires: #( 'bloc:v3.0.x' ).
	self version: 'v3.0.0' of: 'bloc' requires: #(  ).
	report := self resolve: 'root:v1.0.0'.
	self assert: (report warnings anySatisfy: [ :each | each kind = #floatingVersion ])
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAWarningOfASupersededVersionIsOnlyHistory [
	"Album v0.10.0 asks for a floating version, but Toplo raises Album to v0.11.0: the warning stays on the requirement, for debugging, and is not a finding"

	| report requirement |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v0.10.0' 'v0.11.0' ) of: 'album'.
	self versions: #( 'v0.11.0' ) of: 'toplo'.
	self versions: #( 'v3.0.0' 'v3.1.0' ) of: 'bloc'.
	self version: 'v1.0.0' of: 'root' requires: #( 'album:v0.10.0' 'toplo:v0.11.0' ).
	self version: 'v0.10.0' of: 'album' requires: #( 'bloc:v3.0.x' ).
	self version: 'v0.11.0' of: 'album' requires: #( 'bloc:v3.1.0' ).
	self version: 'v0.11.0' of: 'toplo' requires: #( 'album:v0.11.0' ).
	self version: 'v3.0.0' of: 'bloc' requires: #(  ).
	self version: 'v3.1.0' of: 'bloc' requires: #(  ).
	report := self resolve: 'root:v1.0.0'.
	requirement := report requirements detect: [ :each | each from version string = 'v0.10.0' ].
	self
		assert: report warnings isEmpty;
		assert: (report isSuperseded: requirement);
		assert: (requirement findings anySatisfy: [ :each | each kind = #floatingVersion ])
]

{ #category : 'tests - requirements' }
MetacelloMVSResolverTest >> testAnErrorIsAttachedToTheRequirementAndTheVersionThatAsked [

	| report requirement |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.0.0' ) of: 'b'.
	self version: 'v1.0.0' of: 'root' requires: #( 'b:v1.3.0' ).
	report := self resolve: 'root:v1.0.0'.
	requirement := report requirements detect: [ :each | each repository = (self repo: 'b') ].
	self
		assert: requirement hasErrors;
		assert: requirement resolved isNil;
		assert: ((report findingsOf: (self repo: 'root')) anySatisfy: [ :each | each kind = #missingVersion ])
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testAnInstalledVersionIsNeverDowngraded [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.2.0' 'v1.5.0' ) of: 'b'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.2.0' ).
	self version: 'v1.2.0' of: 'b' requires: #(  ).
	self version: 'v1.5.0' of: 'b' requires: #(  ).
	buildList := ((MetacelloMVSResolver on: graph)
		              installed: { ((self repo: 'b') -> (self installed: (self repo: 'b:v1.5.0') project: 'b')) } asDictionary;
		              resolve: (self repo: 'a:v1.0.0/src') project: 'a') buildList.
	self
		assert: (self selected: 'b' in: buildList) equals: 'v1.5.0';
		assert: (buildList at: (self repo: 'b')) chainString equals: '  installed github://o/b v1.5.0'
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testAnInstalledVersionKeepsItsPathAndProject [

	| installedSpec buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.2.0' 'v1.5.0' ) of: 'b'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.2.0' ).
	self version: 'v1.2.0' of: 'b' requires: #(  ).
	self version: 'v1.5.0' of: 'b' requires: #(  ).
	installedSpec := graph specFor: (self repo: 'b:v1.5.0/src') project: 'B' loads: #(  ).
	buildList := ((MetacelloMVSResolver on: graph)
		              installed: { ((self repo: 'b') -> installedSpec) } asDictionary;
		              resolve: (self repo: 'a:v1.0.0/src') project: 'a') buildList.
	self
		assert: (buildList at: (self repo: 'b')) path equals: 'src';
		assert: (buildList at: (self repo: 'b')) project equals: 'B'
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAnOlderHigherPatchIsAWarning [
	"v1.2.4 points to a commit older than v1.2.3's: a retag or a misplaced tag"

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.2.3' 'v1.2.4' ) of: 'lib'.
	graph repository: (self repo: 'lib') dates: {
			('v1.2.3' -> (self date: '2024-05-01')).
			('v1.2.4' -> (self date: '2023-01-01')) } asDictionary.
	self version: 'v1.0.0' of: 'root' requires: #( 'lib:v1.2.3' ).
	self version: 'v1.2.3' of: 'lib' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root'.
	self assert: report resolves.
	self
		assert: (report descriptionOf: report warnings first)
		equals: (String lf join: #( 'github://o/lib v1.2.3 (2024-05-01) is newer than v1.2.4 (2023-01-01). Required by:'
				    '  github://o/root v1.0.0' ))
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAnOlderMinorOrMajorIsFine [
	"A v1.2.5 published after v1.3.0, or a v2.1.0 after v3.0.0, is a backport, not an inversion"

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.2.5' 'v1.3.0' 'v2.1.0' 'v3.0.0' ) of: 'lib'.
	graph repository: (self repo: 'lib') dates: {
			('v1.2.5' -> (self date: '2024-06-01')).
			('v1.3.0' -> (self date: '2024-01-01')).
			('v2.1.0' -> (self date: '2024-09-01')).
			('v3.0.0' -> (self date: '2024-03-01')) } asDictionary.
	self version: 'v1.0.0' of: 'root' requires: #( 'lib:v1.2.5' ).
	self version: 'v1.2.5' of: 'lib' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root'.
	self assert: report warnings isEmpty
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testAnotherCapitalizationIsTheSameRepositoryWithAWarning [

	| report buildList |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.0.0' ) of: 'tool'.
	graph repository: 'github://upstream/bloc' versions: #( 'v3.0.0' 'v3.1.0' ).
	self version: 'v1.0.0' of: 'root' requires: {
			'tool:v1.0.0'.
			(self project: 'Bloc' at: 'upstream/bloc:v3.0.0') }.
	self version: 'v1.0.0' of: 'tool' requires: { (self project: 'Bloc' at: 'Upstream/Bloc:v3.1.0') }.
	graph repository: 'github://upstream/bloc' at: 'v3.0.0' requires: #(  ).
	graph repository: 'github://upstream/bloc' at: 'v3.1.0' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root'.
	self assert: report resolves.
	self
		assert: (report descriptionOf: report warnings first)
		equals:
			(String lf join:
				 #( 'github://Upstream/Bloc:v3.1.0/src spells github://upstream/bloc with another capitalization. Required by:'
				    '  github://o/root v1.0.0' '  github://o/tool v1.0.0' )).
	buildList := (self resolve: 'root:v1.0.0') buildList.
	self
		assert: buildList size equals: 3;
		assert: (buildList at: 'github://upstream/bloc') version string equals: 'v3.1.0'
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testChronologyIsCheckedOncePerRepository [

	| report |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.0.0' ) of: 'other'.
	self versions: #( 'v1.2.3' 'v1.2.4' ) of: 'lib'.
	graph repository: (self repo: 'lib') dates: {
			('v1.2.3' -> (self date: '2024-05-01')).
			('v1.2.4' -> (self date: '2023-01-01')) } asDictionary.
	self version: 'v1.0.0' of: 'root' requires: #( 'lib:v1.2.3' 'other:v1.0.0' ).
	self version: 'v1.0.0' of: 'other' requires: #( 'lib:v1.2.4' ).
	self version: 'v1.2.3' of: 'lib' requires: #(  ).
	self version: 'v1.2.4' of: 'lib' requires: #(  ).
	report := (MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root'.
	self assert: report warnings size equals: 1
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testEveryRootIsResolvedInOrder [
	"Each root is loaded even when nothing requires it, in the order asked"
	| resolution |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' ) of: 'b'.
	self version: 'v1.0.0' of: 'a' requires: #(  ).
	self version: 'v1.0.0' of: 'b' requires: #(  ).
	resolution := self resolveRoots: #( 'b:v1.0.0' 'a:v1.0.0' ).
	self assert: resolution resolves.
	self assert: (resolution rootCandidates collect: [ :each | each repository ]) asArray equals: {
			(self repo: 'b').
			(self repo: 'a') }
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testNeverSelectsMoreThanNeeded [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' 'v1.2.0' 'v1.5.0' ) of: 'b'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.2.0' ).
	self version: 'v1.2.0' of: 'b' requires: #(  ).
	buildList := (self resolve: 'a:v1.0.0') buildList.
	self assert: (self selected: 'b' in: buildList) equals: 'v1.2.0'
]

{ #category : 'tests - problems' }
MetacelloMVSResolverTest >> testNoProblemsForAGraphThatResolves [

	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.0.0' ) of: 'a'.
	self version: 'v1.0.0' of: 'root' requires: #( 'a:v1.0.0' ).
	self version: 'v1.0.0' of: 'a' requires: #(  ).
	self assert: ((MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root') errors isEmpty
]

{ #category : 'tests - dev mode' }
MetacelloMVSResolverTest >> testNothingToLoadIsAnError [

	self
		assert: (((MetacelloMVSResolver on: graph) resolveRoots: #(  )) errors collect: [ :each | each kind ]) asArray
		equals: #( #invalidReference )
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testPrereleasesOnlyWhenRequired [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' 'v1.1.0-rc1' 'v1.1.0' ) of: 'b'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.0.1' ).
	self version: 'v1.1.0' of: 'b' requires: #(  ).
	self version: 'v1.1.0-rc1' of: 'b' requires: #(  ).
	buildList := (self resolve: 'a:v1.0.0') buildList.
	self assert: (self selected: 'b' in: buildList) equals: 'v1.1.0'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.1.0-rc1' ).
	buildList := (self resolve: 'a:v1.0.0') buildList.
	self assert: (self selected: 'b' in: buildList) equals: 'v1.1.0-rc1'
]

{ #category : 'tests - problems' }
MetacelloMVSResolverTest >> testProblemsListsEveryErrorOfTheGraph [
	"Like Moose: a branch, a missing ref and a major conflict in one graph"

	| problems |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' 'v2.0.0' ) of: 'c'.
	self version: 'v1.0.0' of: 'root' requires: #( 'a:v1.0.0' 'b:development' 'c:v2.0.0' ).
	self version: 'v1.0.0' of: 'a' requires: #( 'c:v1.0.0' 'd' ).
	self version: 'v2.0.0' of: 'c' requires: #(  ).
	problems := ((MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root') errors.
	self assert: (problems collect: [ :each | each kind ]) asArray equals: {
			#unversionedReference.
			#majorConflict.
			#unversionedReference }.
	self assert: (problems first text beginsWith: 'github://o/b:development/src names no version').
	self assert: (problems last text beginsWith: 'github://o/d/src names no version')
]

{ #category : 'tests - problems' }
MetacelloMVSResolverTest >> testProblemsReportsAProblemOnce [
	"Like XMLParser, whose Tests and Tools aliases require the same thing again"

	| problems |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self version: 'v1.0.0' of: 'root' requires: #( 'dictionary:development' 'dictionary:development' ).
	problems := ((MetacelloMVSResolver on: graph) resolve: (self repo: 'root:v1.0.0') project: 'root') errors.
	self assert: problems size equals: 1
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testRequirementsOfVersionsNotSelectedStillCountWhenUnusedAreKept [
	"As in Go: D 1.3 is not selected, but what it requires is"

	| buildList |
	self givenADependencyOfAVersionNotSelected.
	buildList := ((MetacelloMVSResolver on: graph)
		              dropUnused: false;
		              resolve: (self repo: 'a:v1.0.0') , '/src' project: 'a') buildList.
	self
		assert: (self selected: 'd' in: buildList) equals: 'v1.4.0';
		assert: (self selected: 'f' in: buildList) equals: 'v1.0.0'
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testRootsShareTheVersionsTheyRequire [
	"Two roots requiring the same project get one version of it, the highest asked, and it is not a root"
	| resolution |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' ) of: 'b'.
	self versions: #( 'v1.0.0' 'v1.1.0' ) of: 'c'.
	self version: 'v1.0.0' of: 'a' requires: #( 'c:v1.0.0' ).
	self version: 'v1.0.0' of: 'b' requires: #( 'c:v1.1.0' ).
	self version: 'v1.0.0' of: 'c' requires: #(  ).
	self version: 'v1.1.0' of: 'c' requires: #(  ).
	resolution := self resolveRoots: #( 'a:v1.0.0' 'b:v1.0.0' ).
	self assert: resolution resolves.
	self assert: (self selected: 'c' in: resolution buildList) equals: 'v1.1.0'.
	self deny: (resolution isRoot: (resolution buildList at: (self repo: 'c')))
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testSelectsTheHighestMinimum [
	"The example of the Go modules paper: B needs D 1.3, C needs D 1.4"

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.1.0' 'v1.2.0' 'v1.3.0' ) of: 'b'.
	self versions: #( 'v1.2.0' 'v1.3.0' ) of: 'c'.
	self versions: #( 'v1.3.0' 'v1.4.0' 'v1.5.0' ) of: 'd'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.2.0' 'c:v1.2.0' ).
	self version: 'v1.2.0' of: 'b' requires: #( 'd:v1.3.0' ).
	self version: 'v1.2.0' of: 'c' requires: #( 'd:v1.4.0' ).
	self version: 'v1.3.0' of: 'd' requires: #(  ).
	self version: 'v1.4.0' of: 'd' requires: #(  ).
	buildList := (self resolve: 'a:v1.0.0') buildList.
	self
		assert: buildList size equals: 4;
		assert: (self selected: 'a' in: buildList) equals: 'v1.0.0';
		assert: (self selected: 'b' in: buildList) equals: 'v1.2.0';
		assert: (self selected: 'c' in: buildList) equals: 'v1.2.0';
		assert: (self selected: 'd' in: buildList) equals: 'v1.4.0'
]

{ #category : 'tests - warnings' }
MetacelloMVSResolverTest >> testTheCanonicalSpellingOfAVersionIsPreferred [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.2' 'v1.2.0' ) of: 'plural'.
	self version: 'v1.0.0' of: 'root' requires: #( 'plural:v1.2.0' ).
	self version: 'v1.2.0' of: 'plural' requires: #(  ).
	buildList := (self resolve: 'root:v1.0.0') buildList.
	self assert: (self selected: 'plural' in: buildList) equals: 'v1.2.0'
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testTheExampleOfThePullRequest [
	"A 1.0 -> B 1.0 -> C 1.2 -> E 3.5
	                 -> D 2.1 -> C 1.3 -> F 4.2
	C 1.3 is selected; E 3.5, which only C 1.2 asks for, is dropped"

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' ) of: 'b'.
	self versions: #( 'v1.2.0' 'v1.3.0' 'v1.4.0' ) of: 'c'.
	self versions: #( 'v2.1.0' 'v2.2.0' ) of: 'd'.
	self versions: #( 'v3.5.0' 'v3.6.0' ) of: 'e'.
	self versions: #( 'v4.2.0' ) of: 'f'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.0.0' ).
	self version: 'v1.0.0' of: 'b' requires: #( 'c:v1.2.0' 'd:v2.1.0' ).
	self version: 'v1.2.0' of: 'c' requires: #( 'e:v3.5.0' ).
	self version: 'v1.3.0' of: 'c' requires: #( 'f:v4.2.0' ).
	self version: 'v2.1.0' of: 'd' requires: #( 'c:v1.3.0' ).
	self version: 'v3.5.0' of: 'e' requires: #(  ).
	self version: 'v4.2.0' of: 'f' requires: #(  ).
	buildList := (self resolve: 'a:v1.0.0') buildList.
	self
		assert: (#( 'a' 'b' 'c' 'd' 'f' ) collect: [ :each | self selected: each in: buildList ])
		equals: #( 'v1.0.0' 'v1.0.0' 'v1.3.0' 'v2.1.0' 'v4.2.0' );
		deny: (buildList includesKey: (self repo: 'e'))
]

{ #category : 'tests - requirements' }
MetacelloMVSResolverTest >> testTheReportKeepsWhatWasAskedAndWhatItResolvedTo [
	"Album v0.10.0 asks Bloc v3.0.0 and is superseded by Album v0.11.0, which asks v3.1.0; Toplo asks v3.0.0 and gets v3.1.0"

	| report asked |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v0.10.0' 'v0.11.0' ) of: 'album'.
	self versions: #( 'v0.11.0' ) of: 'toplo'.
	self versions: #( 'v3.0.0' 'v3.1.0' ) of: 'bloc'.
	self version: 'v1.0.0' of: 'root' requires: #( 'album:v0.10.0' 'toplo:v0.11.0' ).
	self version: 'v0.10.0' of: 'album' requires: #( 'bloc:v3.0.0' ).
	self version: 'v0.11.0' of: 'album' requires: #( 'bloc:v3.1.0' ).
	self version: 'v0.11.0' of: 'toplo' requires: #( 'album:v0.11.0' 'bloc:v3.0.0' ).
	self version: 'v3.0.0' of: 'bloc' requires: #(  ).
	self version: 'v3.1.0' of: 'bloc' requires: #(  ).
	report := self resolve: 'root:v1.0.0'.
	asked := report requirements select: [ :each | each repository = (self repo: 'bloc') ].
	self assert: (asked collect: [ :each | each from printString -> (graph versionStringOf: each asked) ]) asArray equals: {
			('github://o/album v0.10.0' -> 'v3.0.0').
			('github://o/toplo v0.11.0' -> 'v3.0.0').
			('github://o/album v0.11.0' -> 'v3.1.0') }.
	self assert: (asked collect: [ :each | each resolved version string ]) asSet equals: #( 'v3.1.0' ) asSet.
	self assert: (asked collect: [ :each | report isSuperseded: each ]) asArray equals: #( true false false ).
	self assert: (report dependenciesOf: (self repo: 'album')) equals: { (self repo: 'bloc') } asSet
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testTheRootCanNameItsProject [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self version: 'v1.0.0' of: 'a' requires: #(  ).
	buildList := ((MetacelloMVSResolver on: graph) resolve: (self repo: 'a:v1.0.0/src') project: 'A') buildList.
	self assert: (buildList at: (self repo: 'a')) project equals: 'A'
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testTheRootMustBeVersioned [

	self assert: ((self resolve: 'a:main') errors anySatisfy: [ :each | each kind = #unversionedReference ])
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testTheSameProjectFromTheSameRepositoryIsFine [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.0.0' ) of: 'tool'.
	graph repository: 'github://upstream/bloc' versions: #( 'v3.0.0' 'v3.1.0' ).
	self version: 'v1.0.0' of: 'root' requires: {
			'tool:v1.0.0'.
			(self project: 'Bloc' at: 'upstream/bloc:v3.0.0') }.
	self version: 'v1.0.0' of: 'tool' requires: { (self project: 'Bloc' at: 'upstream/bloc:v3.1.0') }.
	graph repository: 'github://upstream/bloc' at: 'v3.0.0' requires: #(  ).
	graph repository: 'github://upstream/bloc' at: 'v3.1.0' requires: #(  ).
	buildList := (self resolve: 'root:v1.0.0') buildList.
	self assert: (buildList at: 'github://upstream/bloc') version string equals: 'v3.1.0'
]

{ #category : 'tests - cases' }
MetacelloMVSResolverTest >> testTwoForksOfAProjectAreAnError [
	"Like gtoolkit's feenkcom/Bloc and pharo-graphics/Bloc: two repositories, one baseline"

	| report error |
	self versions: #( 'v1.0.0' ) of: 'root'.
	self versions: #( 'v1.0.0' ) of: 'tool'.
	graph repository: 'github://fork/bloc' versions: #( 'v0.9.119' ).
	graph repository: 'github://upstream/bloc' versions: #( 'v3.0.0' ).
	self version: 'v1.0.0' of: 'root' requires: {
			'tool:v1.0.0'.
			(self project: 'Bloc' at: 'upstream/bloc:v3.0.0') }.
	self version: 'v1.0.0' of: 'tool' requires: { (self project: 'Bloc' at: 'fork/bloc:v0.9.119') }.
	graph repository: 'github://upstream/bloc' at: 'v3.0.0' requires: #(  ).
	graph repository: 'github://fork/bloc' at: 'v0.9.119' requires: #(  ).
	error := (report := self resolve: 'root:v1.0.0') errors detect: [ :each | each kind = #projectConflict ] ifNone: [ nil ].
	self assert: error isNotNil.
	self assert: ((report descriptionOf: error) beginsWith: 'The project Bloc is required from two repositories:').
	self assert:
		((report descriptionOf: error) includesSubstring: (String lf join: #( '  github://o/root v1.0.0' '  github://upstream/bloc v3.0.0' ))).
	self assert:
		((report descriptionOf: error) includesSubstring: (String lf join: #( '  github://o/tool v1.0.0' '  github://fork/bloc v0.9.119' )))
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testTwoMajorsAreAnError [

	| report error |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' ) of: 'b'.
	self versions: #( 'v1.4.0' 'v2.1.0' ) of: 'c'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.0.0' 'c:v2.1.0' ).
	self version: 'v1.0.0' of: 'b' requires: #( 'c:v1.4.0' ).
	self version: 'v2.1.0' of: 'c' requires: #(  ).
	self version: 'v1.4.0' of: 'c' requires: #(  ).
	error := (report := self resolve: 'a:v1.0.0') errors detect: [ :each | each kind = #majorConflict ] ifNone: [ nil ].
	self assert: error isNotNil.
	self assert: ((report descriptionOf: error) includesSubstring: (String lf join: #( '  github://o/a v1.0.0' '  github://o/c v2.1.0' ))).
	self assert: ((report descriptionOf: error) includesSubstring: (String lf join: #( '  github://o/b v1.0.0' '  github://o/c v1.4.0' )))
]

{ #category : 'tests' }
MetacelloMVSResolverTest >> testWhyShowsWhoRequiredTheSelectedVersion [

	| buildList |
	self versions: #( 'v1.0.0' ) of: 'a'.
	self versions: #( 'v1.0.0' ) of: 'b'.
	self versions: #( 'v1.3.0' 'v1.4.0' ) of: 'd'.
	self version: 'v1.0.0' of: 'a' requires: #( 'b:v1.0.0' 'd:v1.3.0' ).
	self version: 'v1.0.0' of: 'b' requires: #( 'd:v1.4.0' ).
	self version: 'v1.4.0' of: 'd' requires: #(  ).
	self version: 'v1.3.0' of: 'd' requires: #(  ).
	buildList := (self resolve: 'a:v1.0.0') buildList.
	self
		assert: (buildList at: (self repo: 'd')) chainString
		equals: (String lf join: #( '  github://o/a v1.0.0' '  github://o/b v1.0.0' '  github://o/d v1.4.0' ))
]

{ #category : 'running' }
MetacelloMVSResolverTest >> version: aVersion of: aName requires: anArray [
	"anArray holds name:ref pairs, like #('b:v1.2.0'), or project specs"

	graph repository: (self repo: aName) at: aVersion requires: (anArray collect: [ :each |
				 each isString
					 ifTrue: [ self class specAt: (self repo: each) , '/src' ]
					 ifFalse: [ each ] ])
]

{ #category : 'running' }
MetacelloMVSResolverTest >> versions: tagArray of: aName [

	graph repository: (self repo: aName) versions: tagArray
]

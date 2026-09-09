"
A Hudson report
"
Class {
	#name : 'HDReport',
	#superclass : 'Object',
	#classVars : [
		'ShuffleRandom'
	],
	#category : 'SUnit-Basic-CLI-Core',
	#package : 'SUnit-Basic-CLI',
	#tag : 'Core'
}

{ #category : 'running' }
HDReport class >> runClasses: aCollectionOfClasses named: aString [
	self subclassResponsibility
]

{ #category : 'running' }
HDReport class >> runPackage: aString [
	self subclassResponsibility
]

{ #category : 'running' }
HDReport class >> runPackages: aCollectionOfStrings [

	"Run the packages in a predictable order.
	We first sort them alphabetically, then we shuffle with the same seed than the tests.
	
	This makes that
	 - packages are run in a random order
	 - all tests of a package are run together, they are not mixed with other packages
	 - tests within a package are run in a random order
	 - all randomization happens with the same seed"
	
	| sortedPackageNames |
	sortedPackageNames := aCollectionOfStrings sorted.
	sortedPackageNames shuffleBy: self shuffleRandom.
	^ aCollectionOfStrings collect: [ :packageName | self runPackage: packageName ]
]

{ #category : 'running' }
HDReport class >> shuffleRandom [

	"If no seed is given, initialize it.
	All suites will use the same seed"
	ShuffleRandom ifNil: [
		ShuffleRandom := Random new ].
	^ ShuffleRandom
]

{ #category : 'running' }
HDReport class >> shuffleSeed [

	^ self shuffleRandom seed
]

{ #category : 'running' }
HDReport class >> shuffleSeed: aSeed [
	
	ShuffleRandom := Random seed: aSeed
]

{ #category : 'private' }
HDReport >> convert: aString [
	^ (aString asString 
		copyReplaceAll: (String with: Character cr with: Character lf) with: (String with: Character lf))
		copyReplaceAll: (String with: Character cr) with: (String with: Character lf)
]

{ #category : 'private' }
HDReport >> encode: aString [
	^ ((aString asString 
		copyReplaceAll: '&' with: '&amp;') 
		copyReplaceAll: '"' with: '&quot;') 
		copyReplaceAll: '<' with: '&lt;'
]

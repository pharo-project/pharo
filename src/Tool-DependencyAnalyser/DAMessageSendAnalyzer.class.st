"
I analyze package sent messages to reveal dependencies.

Examples:
(self on: 'Kernel') difference
(self on: 'Kernel') missingMethodsWithPotentialMatch
(self on: 'Kernel') possibleDeadCode
(self on: 'Kernel') missingDependencies
(self on: 'Kernel') 	missingMethodsWithPotentialMatchAfterMissingDependenciesAddition

"
Class {
	#name : #DAMessageSendAnalyzer,
	#superclass : #Object,
	#instVars : [
		'packageAnalysis',
		'missingMethods'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #'instance creation' }
DAMessageSendAnalyzer class >> on: aPackageName [
	| packageAnalysis |
	packageAnalysis := ((DAPackageRelationGraph onPackagesNamed: { aPackageName })
				build;
				packages)
			select: #isSeen.
	^ self basicNew
		initializeWith: packageAnalysis first;
		yourself
]

{ #category : #utilities }
DAMessageSendAnalyzer class >> packagesProvidingSelector: aSelector [
	^ aSelector implementors
		collect: [:each | each package name]
		as: Set
]

{ #category : #computing }
DAMessageSendAnalyzer >> implementedMessages [

	^ (self packageStaticDependencies
		   add: self packageName;
		   yourself) flatCollect: [ :name | (Smalltalk organization packageNamed: name) selectors ]
]

{ #category : #computing }
DAMessageSendAnalyzer >> implementedMessagesWithManuallyResolvedDependencies [

	^ (self manuallyResolvedDependencies flatCollect: [ :name | (Smalltalk organization packageNamed: name) selectors ] as: Set)
		  addAll: self implementedMessages;
		  yourself
]

{ #category : #initialization }
DAMessageSendAnalyzer >> initializeWith: aDAPackage [
	packageAnalysis := aDAPackage
]

{ #category : #accessing }
DAMessageSendAnalyzer >> manuallyResolvedDependencies [
	^ self rPackage manuallyResolvedDependencies
]

{ #category : #computing }
DAMessageSendAnalyzer >> missingDependencies [
	^ (self missingMethodsWithPotentialMatch
		select: [ :each | each size = 1 ]) flattened asSet
]

{ #category : #accessing }
DAMessageSendAnalyzer >> missingMethods [
	^ self missingMethodsWithPotentialMatch keys
]

{ #category : #computing }
DAMessageSendAnalyzer >> missingMethodsImplementedIn: aPackageName [

	| rPackage |
	rPackage := Smalltalk organization packageNamed: aPackageName ifAbsent: [
		            DAUnknownManuallyResolvedPackage signalOn: aPackageName.
		            ^ self missingMethods ].

	^ rPackage selectors intersection: self missingMethods
]

{ #category : #computing }
DAMessageSendAnalyzer >> missingMethodsWithPotentialMatch [
	^ missingMethods ifNil:
		[ missingMethods := Dictionary new.
		self unimplementedCalls do: [ :selector |
		missingMethods
			at: selector
			put: (self packagesProvidingSelector: selector) ].
		missingMethods ]
]

{ #category : #computing }
DAMessageSendAnalyzer >> missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition [
	| dictionary |

	dictionary := Dictionary new.
	self unimplementedCallsInAllDependencies do: [ :selector |
		dictionary
			at: selector
			put: (self packagesProvidingSelector: selector) ].
	^ dictionary
]

{ #category : #accessing }
DAMessageSendAnalyzer >> packageName [
	^ packageAnalysis packageName
]

{ #category : #accessing }
DAMessageSendAnalyzer >> packageStaticDependencies [
	^ packageAnalysis dependencies
		collect: [:each | each target packageName ]
		as: Set
]

{ #category : #computing }
DAMessageSendAnalyzer >> packagesProvidingSelector: aSelector [
	^ self class packagesProvidingSelector: aSelector
]

{ #category : #computing }
DAMessageSendAnalyzer >> possibleDeadCode [
	^ self missingMethodsWithPotentialMatch
		select: #isEmpty
]

{ #category : #accessing }
DAMessageSendAnalyzer >> rPackage [
	"Answer a <RPackage> matching the receiver's package name. If we are browsing an unloaded package, then answer a virtual RPackage so the message send analyzer could preserve its behavior with loaded packages in #manuallyResolvedDependencies"

	^ packageAnalysis rPackageSet packages
		ifNotEmpty: [ : rPackages | rPackages detect: [ :each | each packageName = self packageName ] ]
		ifEmpty: [ RPackage named: 'Virtual' , self packageName ]
]

{ #category : #computing }
DAMessageSendAnalyzer >> sentMessages [
	^ self rPackage methods
		flatCollect: #messages
		as: Set
]

{ #category : #computing }
DAMessageSendAnalyzer >> unimplementedCalls [
	^ self sentMessages difference: self implementedMessages
]

{ #category : #computing }
DAMessageSendAnalyzer >> unimplementedCallsInAllDependencies [
	^ self sentMessages difference: self implementedMessagesWithManuallyResolvedDependencies
]

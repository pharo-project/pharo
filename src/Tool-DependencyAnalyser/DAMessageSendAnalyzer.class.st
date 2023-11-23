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
	#name : 'DAMessageSendAnalyzer',
	#superclass : 'Object',
	#instVars : [
		'packageAnalysis',
		'missingMethods'
	],
	#category : 'Tool-DependencyAnalyser-Core',
	#package : 'Tool-DependencyAnalyser',
	#tag : 'Core'
}

{ #category : 'instance creation' }
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

{ #category : 'utilities' }
DAMessageSendAnalyzer class >> packagesProvidingSelector: aSelector [
	^ aSelector implementors
		collect: [:each | each package name]
		as: Set
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> implementedMessages [

	^ (self packageStaticDependencies
		   add: self packageName;
		   yourself) flatCollect: [ :name | (self packageOrganizer packageNamed: name) selectors ]
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> implementedMessagesWithManuallyResolvedDependencies [

	^ (self manuallyResolvedDependencies flatCollect: [ :name | (self packageOrganizer packageNamed: name) selectors ] as: Set)
		  addAll: self implementedMessages;
		  yourself
]

{ #category : 'initialization' }
DAMessageSendAnalyzer >> initializeWith: aDAPackage [
	packageAnalysis := aDAPackage
]

{ #category : 'accessing' }
DAMessageSendAnalyzer >> manuallyResolvedDependencies [

	^ self systemPackage manuallyResolvedDependencies
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> missingDependencies [
	^ (self missingMethodsWithPotentialMatch
		select: [ :each | each size = 1 ]) flattened asSet
]

{ #category : 'accessing' }
DAMessageSendAnalyzer >> missingMethods [
	^ self missingMethodsWithPotentialMatch keys
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> missingMethodsImplementedIn: aPackageName [

	| rPackage |
	rPackage := self packageOrganizer packageNamed: aPackageName ifAbsent: [
		            DAUnknownManuallyResolvedPackage signalOn: aPackageName.
		            ^ self missingMethods ].

	^ rPackage selectors intersection: self missingMethods
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> missingMethodsWithPotentialMatch [
	^ missingMethods ifNil:
		[ missingMethods := Dictionary new.
		self unimplementedCalls do: [ :selector |
		missingMethods
			at: selector
			put: (self packagesProvidingSelector: selector) ].
		missingMethods ]
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition [
	| dictionary |

	dictionary := Dictionary new.
	self unimplementedCallsInAllDependencies do: [ :selector |
		dictionary
			at: selector
			put: (self packagesProvidingSelector: selector) ].
	^ dictionary
]

{ #category : 'accessing' }
DAMessageSendAnalyzer >> packageName [
	^ packageAnalysis packageName
]

{ #category : 'accessing' }
DAMessageSendAnalyzer >> packageStaticDependencies [
	^ packageAnalysis dependencies
		collect: [:each | each target packageName ]
		as: Set
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> packagesProvidingSelector: aSelector [
	^ self class packagesProvidingSelector: aSelector
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> possibleDeadCode [
	^ self missingMethodsWithPotentialMatch
		select: #isEmpty
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> sentMessages [

	^self systemPackage methods flatCollect: #messages as: Set
]

{ #category : 'accessing' }
DAMessageSendAnalyzer >> systemPackage [
	"Answer a <Package> matching the receiver's package name. If we are browsing an unloaded package, then answer a virtual RPackage so the message send analyzer could preserve its behavior with loaded packages in #manuallyResolvedDependencies"

	^ packageAnalysis systemPackage ifNil: [ Package named: 'Virtual' , self packageName ]
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> unimplementedCalls [
	^ self sentMessages difference: self implementedMessages
]

{ #category : 'computing' }
DAMessageSendAnalyzer >> unimplementedCallsInAllDependencies [
	^ self sentMessages difference: self implementedMessagesWithManuallyResolvedDependencies
]

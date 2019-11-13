"
I contain tests that validate the integrity of the system.
"
Class {
	#name : #ReleaseTest,
	#superclass : #AbstractEnvironmentTestCase,
	#category : #'ReleaseTests-Release'
}

{ #category : #accessing }
ReleaseTest class >> actualProcesses [
	^ Process allSubInstances reject: [ :each | each isTerminated ]
]

{ #category : #accessing }
ReleaseTest class >> defaultTimeLimit [

	^ 1 minute
]

{ #category : #accessing }
ReleaseTest class >> knownProcesses [
	"Return a dynamic list of know processes"
	
	"(self actualProcesses difference: self knownProcesses) do: [ :each | each suspend; terminate ]."
	
	^ {
		Processor activeProcess.	
		InputEventFetcher default fetcherProcess.
		WeakArray runningFinalizationProcess.
		Processor backgroundProcess.
		SmalltalkImage current lowSpaceWatcherProcess.
		UIManager default uiProcess.
		Delay schedulingProcess } asSet
]

{ #category : #accessing }
ReleaseTest >> actualProcesses [
	^ self class actualProcesses
]

{ #category : #helpers }
ReleaseTest >> assertValidLintRule: aLintRule [
	self assertValidLintRule: aLintRule withExceptions: {}
]

{ #category : #helpers }
ReleaseTest >> assertValidLintRule: aLintRule withExceptions: someNames [
	| runner results |
	runner := ReSmalllintChecker new.
	runner
		rule: {aLintRule};
		environment: RBBrowserEnvironment default;
		run.

	results := (runner criticsOf: aLintRule) reject: [ :critique | someNames includes: critique entity name ].

	self
		assert: results isEmpty
		description: [ String
				streamContents: [ :s | 
					s
						<< aLintRule rationale;
						lf;
						<< 'Violations: ';
						lf.
					results
						do: [ :e | 
							s
								<< '- ';
								print: e entity ]
						separatedBy: [ s lf ] ] ]
]

{ #category : #helpers }
ReleaseTest >> hasStartUpOrShutDownMethod: aClass [
	| keySelectors |
	keySelectors := #(#startUp #startUp: #shutDown #shutDown:).
	^ (aClass selectors includesAny: keySelectors)
		or: [ aClass class selectors includesAny: keySelectors ]
]

{ #category : #accessing }
ReleaseTest >> knownProcesses [
	^ self class knownProcesses
]

{ #category : #tests }
ReleaseTest >> testAllClassPoolBindingAreClassVariables [
	| wrong |
	wrong := OrderedCollection new.
	testingEnvironment allClasses do: [ :class | wrong addAll: (class classVariables reject: [ :each | each isKindOf: ClassVariable ]) ].
	self assertEmpty: wrong
]

{ #category : #tests }
ReleaseTest >> testAllGlobalBindingAreGlobalVariables [
	| wrong |
	wrong := testingEnvironment associations reject: [ :each | each isKindOf: GlobalVariable ].
	self assertEmpty: wrong
]

{ #category : #tests }
ReleaseTest >> testExplicitRequirementMethodsShouldBeImplementedInTheirUsers [
	"If a class is using a trait with an explicit requirement method, this class should implement the method"
	
	self assertValidLintRule: RBExplicitRequirementMethodsRule new
	
]

{ #category : #tests }
ReleaseTest >> testInstalledMethodsWithIncorrectTrailerKind [
	| incorrectMethods |
	
	incorrectMethods := SystemNavigation new installedMethodsWithIncorrectTrailerKind.
	
	self 
		assert: incorrectMethods isEmpty
		description: [ String streamContents: [ :s|
			s 
				nextPutAll: 'Found methods with incorrect trailer kind: ';
				print: incorrectMethods ]].
	
]

{ #category : #tests }
ReleaseTest >> testInstanceSideMethodsWithNilKeyInLastLiteral [
	| instanceSideMethodsWithNilKeyInLastLiteral |
	
	instanceSideMethodsWithNilKeyInLastLiteral := SystemNavigation new instanceSideMethodsWithNilKeyInLastLiteral.
	
	self 
		assert: instanceSideMethodsWithNilKeyInLastLiteral isEmpty
		description: [ String streamContents: [ :s|
			s 
				nextPutAll: 'Found methods with nil as last literal: ';
				print: instanceSideMethodsWithNilKeyInLastLiteral ]].
	
]

{ #category : #tests }
ReleaseTest >> testKeyClassesArePresentInStartupList [
	| keyClasses registeredHandlers |
	keyClasses := #(#Delay #ProcessorScheduler #Stdio #OSPlatform #UUIDGenerator #DiskStore #WeakArray #BasicCommandLineHandler).
	registeredHandlers := SessionManager default startupList
		collect: #handledId.
	
	keyClasses do:	[ :className |
		self assert: (registeredHandlers includes: className).
		self assert: (self hasStartUpOrShutDownMethod: (testingEnvironment at: className)) ].
	
	self assert: (registeredHandlers includes: #UIManagerSessionHandler).
]

{ #category : #tests }
ReleaseTest >> testLayoutIntegrity [
	SystemNavigation default allClassesDo: [ :class |
		class instanceSide classLayout checkIntegrity.
		class classSide classLayout checkIntegrity ]
]

{ #category : #tests }
ReleaseTest >> testLocalMethodsOfTheClassShouldNotBeRepeatedInItsTraits [
	"If a class is using a trait with an explicit requirement method, this class should implement the method"
	
	self assertValidLintRule: RBLocalMethodsSameThanTraitRule new withExceptions: #(RBSmalllintTestLocalMethodsSameThanTraitObject).
]

{ #category : #tests }
ReleaseTest >> testMethodsWithUnboundGlobals [
	| methodsWithUnboundGlobals |
	"Ensure the environment is clean"
	3 timesRepeat: [ Smalltalk garbageCollect ].
	
	Smalltalk cleanOutUndeclared. 
	methodsWithUnboundGlobals := SystemNavigation new methodsWithUnboundGlobals.
	
	self 
		assert: methodsWithUnboundGlobals isEmpty
		description: [ String streamContents: [ :s |
			s 
				nextPutAll: 'Found methods with unbound globals: '; 
				print: methodsWithUnboundGlobals ]]
]

{ #category : #tests }
ReleaseTest >> testNoEmptyPackages [
	"Test that we have no empty packages left"

	| violating |
	violating := RPackageOrganizer default packages select: #isEmpty.
	self assertEmpty: violating
]

{ #category : #tests }
ReleaseTest >> testNoNilAssignmentInInitializeMethod [
	
	self assertValidLintRule: ReNoNilAssignationInInitializeRule new
	
]

{ #category : #tests }
ReleaseTest >> testObsoleteClasses [
	| obsoleteClasses |
	
	Smalltalk fixObsoleteReferences.
	Smalltalk garbageCollect.
	obsoleteClasses := SystemNavigation new obsoleteClasses
		select: [ :each | each isAnonymous not ].
	
	self 
		assert: obsoleteClasses isEmpty
		description: [
			String streamContents: [ :s|
				s 
					nextPutAll: 'Obsolete classes remaining: ';
					print: obsoleteClasses ]]
]

{ #category : #tests }
ReleaseTest >> testPharoVersionFileExists [

	"Test there is a pharo.version file next to this image containing the short version of this pharo image.
	This file is required by the Pharo launcher to correctly detect the Pharo version we are running on."
	
	| pharoVersionFile |
	pharoVersionFile := FileLocator imageDirectory / 'pharo.version'.
	self assert: pharoVersionFile exists.
	self
		assert: pharoVersionFile readStream contents trimBoth
		equals: SystemVersion current major asString,  SystemVersion current minor asString
]

{ #category : #'tests - rpackage' }
ReleaseTest >> testRPackageOrganizer [
	"Ensure other tests temporary created organizers are collected"
	3 timesRepeat: [ Smalltalk garbageCollect ].

	"Now check :)"
	self 
		assert: RPackageOrganizer allInstances size = 1
		description: 'There are multiple (', RPackageOrganizer allInstances size asString, ') instances of RPackageOrganizer'.
		  
	self 
		assert: RPackageOrganizer allInstances first == RPackageOrganizer default 
		description: 'The default package organizer is the not the only instance of RPackageOrganizer'.
]

{ #category : #tests }
ReleaseTest >> testShouldFileLocatorBeBeforeSystemSettingsPersistenceInStartupList [
	"This test documents issue https://pharo.manuscript.com/f/cases/17721"
	| startupList settingsIndex |
	
	startupList := (SessionManager default startupList collect: #handledId).
	
	settingsIndex := startupList indexOf: #SystemSettingsPersistence.
	settingsIndex > 0
		ifTrue: [ self should: [ (startupList indexOf: #FileLocator) < (startupList indexOf: #SystemSettingsPersistence) ] ]
]

{ #category : #tests }
ReleaseTest >> testShouldWorldMorphBeAfterFontClassesInStartupList [
	"This test documents issue https://pharo.manuscript.com/f/cases/17834"
	"The Startup of he WorldMorph is now done in the UIManagerHandler"
	| startupList |
	
	startupList := (SessionManager default startupList collect: #handledId).
	
	#(StrikeFont LogicalFont FreeTypeSettings FreeTypeCache FT2Handle) 
		do: [ :fontClass |
			self should: [ (startupList indexOf: #UIManagerSessionHandler) > (startupList indexOf: #SystemSettingsPersistence) ] ]
]

{ #category : #tests }
ReleaseTest >> testTestCasesEndsWithTestOrTestCase [
	self assertValidLintRule: ReTestClassNameShouldEndWithTest new
	
]

{ #category : #'tests - rpackage' }
ReleaseTest >> testThatAllMethodsArePackaged [
	| classes instanceMethods classMethods allMethods methodsWithoutPackageInfo |
	classes := Smalltalk allClassesAndTraits.
	instanceMethods := classes flatCollect: #methods.
	classMethods := classes flatCollect: [ :class | class classSide methods ].
	allMethods := instanceMethods , classMethods.
	methodsWithoutPackageInfo := allMethods select: [ :method | method package isNil ].
	self assertEmpty: methodsWithoutPackageInfo
]

{ #category : #tests }
ReleaseTest >> testUndeclared [
	| undeclaredBindings |
	
	Smalltalk cleanOutUndeclared. 
	undeclaredBindings := Undeclared keys reject: [:each |
			each includesSubstring: 'undeclaredStub' caseSensitive: false].
	
	self 
		assert: undeclaredBindings isEmpty 
		description: (String streamContents: [ :s|
			s 
				nextPutAll: 'Found undeclared references: ';
				print: undeclaredBindings ])
]

{ #category : #tests }
ReleaseTest >> testUnknownProcesses [
	"Make sure that there are no other processes running except the known processes"
	
	| unknownProcesses |
	unknownProcesses := self actualProcesses difference: self knownProcesses.
	self 
		assert: unknownProcesses size <=2
		description: (String streamContents: [ :out |
			out << 'Found unknown processes: '.
			unknownProcesses 
				do: [ :each | out print: each ] 
				separatedBy: [ out << ', ' ] ])
]

{ #category : #'tests - rpackage' }
ReleaseTest >> testUnpackagedClasses [
	| unpackagedClasses |
	unpackagedClasses := Smalltalk allClassesAndTraits select: [:each | 
		(RPackageOrganizer default packageOf: each) packageName = RPackage defaultPackageName ].
	self 
		assert: unpackagedClasses isEmpty 
		description: (String streamContents: [ :s|
			s nextPutAll: 'Found the following unpackaged classes: '.
			unpackagedClasses
				do: [ :cls| s tab print: cls ]
				separatedBy: [ s cr ]])

]

{ #category : #'tests - rpackage' }
ReleaseTest >> testUnpackagedPackageShouldBeEmpty [
	| unpackagePackage |
	unpackagePackage := RPackageOrganizer default packageNamed: RPackage defaultPackageName.
	"The unpackage package should not have any defined class or extended classes"
	self assertEmpty: unpackagePackage classes
]

{ #category : #tests }
ReleaseTest >> testWorldMenuHasHelpForAllEntries [
	"In this test we check that at least every terminal menu entry of the world menu has an help."

	| menuElements |
	menuElements := WorldState new menuBuilder itemList.

	"Here we have the roots of the menu. We want the terminal menu entries."
	[ menuElements allSatisfy: [ :each | each itemList isNil ] ]
		whileFalse: [ menuElements := menuElements inject: OrderedCollection new into: [ :coll :each | each itemList ifNil: [ coll add: each ] ifNotNil: [ :items | coll addAll: items ]. coll ] ].
			
	menuElements collect: #spec thenDo: [ :item | self assert: item help isNotNil description: item label , ' menu entry in world menu should have an help.' ]
]

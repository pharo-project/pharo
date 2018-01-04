"
I contain tests that validate the integrity of the system.
"
Class {
	#name : #ReleaseTest,
	#superclass : #TestCase,
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

{ #category : #testing }
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

{ #category : #testing }
ReleaseTest >> testAllClassPoolBindingAreClassVariables [
	| wrong |
	wrong := OrderedCollection new.
	Smalltalk globals allClasses do: [:class | 
		 wrong addAll: (class classVariables reject: [ :each | each isKindOf:  ClassVariable]).
		].
	self assert: wrong isEmpty.
]

{ #category : #testing }
ReleaseTest >> testAllGlobalBindingAreGlobalVariables [
	| wrong |
	wrong := Smalltalk globals associations reject: [ :each | each isKindOf:  GlobalVariable].
	self assert: wrong isEmpty.
]

{ #category : #testing }
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

{ #category : #testing }
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

{ #category : #testing }
ReleaseTest >> testKeyClassesArePresentInStartupList [
	| keyClasses registeredHandlers |
	keyClasses := #(#Delay #ProcessorScheduler #InputEventFetcher #Stdio #OSPlatform #UUIDGenerator #DiskStore #WeakArray #BasicCommandLineHandler).
	registeredHandlers := SessionManager default startupList
		collect: #handledId.
	
	keyClasses do:	[ :className |
		self assert: (registeredHandlers includes: className).
		self assert: (self hasStartUpOrShutDownMethod: (Smalltalk globals at: className)) ].
	
	self assert: (registeredHandlers includes: #UIManagerSessionHandler).
]

{ #category : #testing }
ReleaseTest >> testLayoutIntegrity [
	SystemNavigation default allClassesDo: [ :class |
		class theNonMetaClass classLayout checkIntegrity.
		class theMetaClass classLayout checkIntegrity ]
]

{ #category : #testing }
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

{ #category : #testing }
ReleaseTest >> testNoEmptyPackages [
	"Test that we have no empty packages left"

	| violating |
	violating := RPackageOrganizer default packages select: #isEmpty.
	self assert: violating isEmpty	
]

{ #category : #testing }
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

{ #category : #'testing - rpackage' }
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

{ #category : #testing }
ReleaseTest >> testShouldFileLocatorBeBeforeSystemSettingsPersistenceInStartupList [
	"This test documents issue https://pharo.fogbugz.com/f/cases/17721/Starting-Image-50617-raises-MNU"
	| startupList settingsIndex |
	
	startupList := (SessionManager default startupList collect: #handledId).
	
	settingsIndex := startupList indexOf: #SystemSettingsPersistence.
	settingsIndex > 0
		ifTrue: [ self should: [ (startupList indexOf: #FileLocator) < (startupList indexOf: #SystemSettingsPersistence) ] ]
]

{ #category : #testing }
ReleaseTest >> testShouldWorldMorphBeAfterFontClassesInStartupList [
	"This test documents issue https://pharo.fogbugz.com/f/cases/17834/World-Morph-is-started-up-before-Fonts"
	| startupList |
	
	startupList := (SessionManager default startupList collect: #handledId).
	
	#(StrikeFont LogicalFont FreeTypeSettings FreeTypeCache FT2Handle) 
		do: [ :fontClass |
			self should: [ (startupList indexOf: #WorldMorph) > (startupList indexOf: #SystemSettingsPersistence) ] ]
]

{ #category : #'testing - rpackage' }
ReleaseTest >> testThatAllMethodsArePackaged [
    | classes instanceMethods classMethods allMethods methodsWithoutPackageInfo|
    classes := Smalltalk allClassesAndTraits.
    instanceMethods := classes flatCollect: #methods.
    classMethods := classes flatCollect: [ :class | class classSide methods ].
    allMethods := instanceMethods , classMethods.
    methodsWithoutPackageInfo := allMethods select: [ :method | method package isNil ].
    self assert: methodsWithoutPackageInfo isEmpty.
]

{ #category : #testing }
ReleaseTest >> testUndeclared [
	| undeclaredReferences |
	
	Smalltalk cleanOutUndeclared. 
	undeclaredReferences := Undeclared.
	
	self 
		assert: undeclaredReferences isEmpty 
		description: (String streamContents: [ :s|
			s 
				nextPutAll: 'Found undeclared references: ';
				print: undeclaredReferences ])
	
]

{ #category : #testing }
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

{ #category : #'testing - rpackage' }
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

{ #category : #testing }
ReleaseTest >> testWorldMenuHasHelpForAllEntries [
	"In this test we check that at least every terminal menu entry of the world menu has an help."

	| menuElements |
	menuElements := WorldState new menuBuilder itemList.

	"Here we have the roots of the menu. We want the terminal menu entries."
	[ menuElements allSatisfy: [ :each | each itemList isNil ] ]
		whileFalse: [ menuElements := menuElements inject: OrderedCollection new into: [ :coll :each | each itemList ifNil: [ coll add: each ] ifNotNil: [ :items | coll addAll: items ]. coll ] ].
			
	menuElements collect: #spec thenDo: [ :item | self assert: item help isNotNil description: item label , ' menu entry in world menu should have an help.' ]
]

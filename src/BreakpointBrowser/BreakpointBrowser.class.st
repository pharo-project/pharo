"
My class method ""open"" opens a Calypso method browser showing all the methods of the image that have halts or breakpoints in them. The different flavours of halts and breakpoints are also covered (break once, break on condition, haltIf: haltOnCount:, haltOnce...).

By default, I do not show methods from my own package, nor methods registered on my ""blacklist"" class method. Blacklisted methods are meant to be methods from the core of Pharo that include halts for legitimate reasons (testing for example) and would probably pollute the experience of users if they were shown in the BreakpointBrowser.
If you want to see all methods containing halts and breakpoints without any filtering, you can call my ""openUnfiltered"" class method. 
"
Class {
	#name : #BreakpointBrowser,
	#superclass : #Object,
	#category : #BreakpointBrowser
}

{ #category : #filtering }
BreakpointBrowser class >> blacklist [
	"These methods will not be shown by BreakpointBrowser by default. They contain halts but are meant to contain them (for example, they can be testing that executing self halt does indeed raise a halt)
	Format: Array of {class name symbol. method name symbol} arrays"
	^ { 
	{#ClyClassWithHalts. #methodWithHalts2}.
	{#ClyClassWithHalts. #methodWithHalts1}.
	{#ClyClassWithProblemMethods. #methodWithHalt}.
	{#ClyClassWithProblemMethods. #methodWithHalt2}.
	{#GTSUnitExampleFailingTest. #testWithHalt}.
	{#Object. #openDebugger}.
	{#Object. #openDebugger:}.
	{#ObjectTest. #b}.
	{#ObjectTest. #b1}.
	{#ObjectTest. #testHaltIf}.
	{#ObjectTest. #testHaltOnce}.
	{#ObjectTest. #testHaltOnCount}.
	{#UndefinedObjectTest. #testHaltIfNil}.
	{#UndefinedObjectTest. #testIfNil}.
	{#UndefinedObjectTest. #testIfNilIfNotNil}.
	{#UndefinedObjectTest. #testIfNotNilIfNil}.
	{#CodeSimulationTests. #methodWithHalt}.
	{#RBRefactoryTestDataApp. #cruft}.
	{#RBSmalllintTestObject. #codeCruftLeftInMethods}.
	{#RBDummyRefactoryTestDataApp. #cruft}.
	 }
]

{ #category : #filtering }
BreakpointBrowser class >> filterOutMethodsFromBlacklist: anOrderedCollectionOfRBMethodDefinition [
	^ anOrderedCollectionOfRBMethodDefinition select: [ :anRGMethodDef | (self methodIsOnBlacklist: anRGMethodDef) not]
]

{ #category : #filtering }
BreakpointBrowser class >> filterOutMethodsFromThisPackage: anOrderedCollectionOfRBMethodDefinition [
	^ anOrderedCollectionOfRBMethodDefinition select: [ :anRGMethodDef |(anRGMethodDef package realPackage == self package) not ]
]

{ #category : #filtering }
BreakpointBrowser class >> methodIsOnBlacklist: anRGMethodDef [
	^ self blacklist anySatisfy: [:classNameSymbolAndMethodNameSymbol |
			((anRGMethodDef realClass) = (self environment at: (classNameSymbolAndMethodNameSymbol at: 1) ifAbsent: [^false])) and: 
			[ anRGMethodDef name = (classNameSymbolAndMethodNameSymbol at: 2) ]
	].
]

{ #category : #filtering }
BreakpointBrowser class >> methodsToDisplay [
	^ SystemNavigation default allMethodsSelect: [ :aMethod | 
		aMethod hasBreakpoint or: [
		aMethod containsHalt]].
]

{ #category : #filtering }
BreakpointBrowser class >> open [
	<script>
	"Opens a method browser showing all methods containing breakpoints (and its flavours like break once), halts (and its flavours like haltOnce), inactive breakpoints (and flavours) and inactive halts (and flavours). Filter out such methods that belong to the HaltManager package."
	| b |
	b := ClyQueryBrowser browseMethods: (self filterOutMethodsFromBlacklist: (self filterOutMethodsFromThisPackage: (self methodsToDisplay))) withTitle: 'Breakpoint Browser'.
	b switchResultTo: (ClySortedQueryResult using: SortMethodsByPackageFunction new )
	
]

{ #category : #filtering }
BreakpointBrowser class >> openUnfiltered [
	<script>
	"Does the same as the openHaltManager method, without filtering out the methods from the HaltManager package"
	| b |
	b := ClyQueryBrowser browseMethods: (self methodsToDisplay) withTitle: 'Breakpoint Browser (unfiltered)'.
	b switchResultTo: (ClySortedQueryResult using: SortMethodsByPackageFunction new )
]

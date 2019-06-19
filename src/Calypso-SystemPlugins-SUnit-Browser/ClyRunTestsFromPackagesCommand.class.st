"
I run all tests from selected packages
"
Class {
	#name : #ClyRunTestsFromPackagesCommand,
	#superclass : #ClyRunTestsFromSelectedItemsCommand,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #activation }
ClyRunTestsFromPackagesCommand class >> contextClass [
	^RPackage asCalypsoItemContext  
]

{ #category : #activation }
ClyRunTestsFromPackagesCommand class >> fullBrowserClassGroupTableIconActivation [
	<classAnnotation>
	
	^ClyTableIconCommandActivation for: ClyClassGroupContextOfFullBrowser 
]

{ #category : #'activation-table  cell' }
ClyRunTestsFromPackagesCommand >> decorateTableCell: anItemCellMorph using: aCommandActivator [
]

{ #category : #execution }
ClyRunTestsFromPackagesCommand >> execute [

	testItems do: [ :each | self runPackageTests: each actualObject ]
]

{ #category : #execution }
ClyRunTestsFromPackagesCommand >> runPackageTests: aPackage [
	| testResult testClasses |
	testClasses := aPackage definedClasses 
		select: [ :each | each isTestCase and: [ each isAbstract not ] ].
	testClasses 
		ifEmpty: [testResult := TestAsserter classForTestResult new]
		ifNotEmpty: [ testResult := testClasses anyOne classForTestResult new].
		
	testClasses do: [ :each | self runTestCase: each results: testResult].		
	testResult updateResultsInHistory.	
	self  
		notifyUserAboutResults: testResult 
		with: aPackage name
]

{ #category : #initialization }
ClyRunTestsFromPackagesCommand >> setUpIconForMenuItem: aMenuItemMorph [


]

"
I provide an abstract class for class group/package 'run tests' commands.
"
Class {
	#name : #ClyRunTestsForPackageOrClassGroupCommand,
	#superclass : #ClyRunTestsFromSelectedItemsCommand,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #execution }
ClyRunTestsForPackageOrClassGroupCommand >> decorateTableCell: anItemCellMorph using: aCommandActivator [
]

{ #category : #execution }
ClyRunTestsForPackageOrClassGroupCommand >> execute [

	testItems do: [ :each | self runTestsOf: each actualObject ]
]

{ #category : #execution }
ClyRunTestsForPackageOrClassGroupCommand >> runTestsOf: aClassGroup [
	| testResult testClasses |
	
	testClasses := aClassGroup classes
		select: [ :each | each isTestCase and: [ each isAbstract not ] ].
	testClasses 
		ifEmpty: [testResult := TestAsserter classForTestResult new]
		ifNotEmpty: [ testResult := testClasses anyOne classForTestResult new].
		
	testClasses do: [ :each | self runTestCase: each results: testResult].		
	testResult updateResultsInHistory.	
	self  
		notifyUserAboutResults: testResult 
		with: aClassGroup name
]

{ #category : #initialization }
ClyRunTestsForPackageOrClassGroupCommand >> setUpIconForMenuItem: aMenuItemMorph [


]

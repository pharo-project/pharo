"
I run all tests from selected classGroup (package tags)
"
Class {
	#name : #ClyRunTestsFromClassGroupCommand,
	#superclass : #ClyRunTestsFromSelectedItemsCommand,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #activation }
ClyRunTestsFromClassGroupCommand class >> contextClass [
	^ ClyClassGroup asCalypsoItemContext
]

{ #category : #execution }
ClyRunTestsFromClassGroupCommand >> decorateTableCell: anItemCellMorph using: aCommandActivator [
]

{ #category : #execution }
ClyRunTestsFromClassGroupCommand >> execute [

	testItems do: [ :each | self runPackageTests: each actualObject ]
]

{ #category : #execution }
ClyRunTestsFromClassGroupCommand >> runPackageTests: aClassGroup [
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
ClyRunTestsFromClassGroupCommand >> setUpIconForMenuItem: aMenuItemMorph [


]

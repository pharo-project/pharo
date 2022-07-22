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

	| testResult |
	testResult := self testRunnerClass new runClasses:
		              aClassGroup classes asOrderedCollection.
	testResult updateResultsInHistory.
	self notifyUserAboutResults: testResult with: aClassGroup name
]

{ #category : #initialization }
ClyRunTestsForPackageOrClassGroupCommand >> setUpIconForMenuItem: aMenuItemMorph [
]

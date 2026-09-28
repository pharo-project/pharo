"
I am a test for ReMoveClassToPackageDriver.

I check that the driver re-packages the class into the package selected by the user, that it aborts without changes when the user cancels the selection, and that it reports when the selected package does not exist.
"
Class {
	#name : 'ReMoveClassToPackageDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'helpers' }
ReMoveClassToPackageDriverTest >> driverFor: aClass [
	| driver application |
	
	application := StApplicationForRefactorDriverMock new.
	driver := (ReMoveClassToPackageDriver newApplication: application) 
		scopes: { RBBrowserEnvironment new } 
		class: aClass.
	self setUpDriver: driver.
	
	^ driver
]

{ #category : 'tests' }
ReMoveClassToPackageDriverTest >> testCancelMoveClass [
	| driver dialog |
	
	driver := self driverFor: RBClassDataForRefactoringTest.
	dialog := MockObject new.
	dialog on: #openModal respond: nil.
	driver packageSelectionDialog: dialog.
	driver runRefactoring.
	
	"demeters law!"
	self assert: driver refactoring model changes changes isEmpty
]

{ #category : 'tests' }
ReMoveClassToPackageDriverTest >> testCurrentPackageIsNotACandidate [
	| driver |
	
	driver := self driverFor: RBClassDataForRefactoringTest.
	
	self deny: (driver candidatePackages includes: RBClassDataForRefactoringTest package).
	self assert: (driver candidatePackages includes: (PackageOrganizer default packageNamed: #'Refactoring-Transformations'))
]

{ #category : 'tests' }
ReMoveClassToPackageDriverTest >> testMoveClassToAnotherPackage [
	| driver dialog |
	
	driver := self driverFor: RBClassDataForRefactoringTest.
	dialog := MockObject new.
	dialog on: #openModal respond: (PackageOrganizer default packageNamed: #'Refactoring-Transformations').
	driver packageSelectionDialog: dialog.
	driver runRefactoring.

	self assert: driver refactoring packageName equals: #'Refactoring-Transformations'.
	"demeters law!"
	self assert: driver refactoring model changes changes size equals: 1
]

{ #category : 'tests' }
ReMoveClassToPackageDriverTest >> testMoveClassToMissingPackageIsReported [
	| driver dialog application |
	
	application := StApplicationForRefactorDriverMock new.
	driver := (ReMoveClassToPackageDriver newApplication: application)
		scopes: { RBBrowserEnvironment new }
		class: RBClassDataForRefactoringTest.
	self setUpDriver: driver.

	dialog := MockObject new.
	dialog on: #openModal respond: (MockObject new
		on: #name respond: #'No-Such-Package-For-Move-Test';
		yourself).
	driver packageSelectionDialog: dialog.
	driver runRefactoring.

	self deny: driver refactoring failedApplicabilityPreconditions isEmpty.
	"demeters law!"
	self assert: driver refactoring model changes changes isEmpty
]

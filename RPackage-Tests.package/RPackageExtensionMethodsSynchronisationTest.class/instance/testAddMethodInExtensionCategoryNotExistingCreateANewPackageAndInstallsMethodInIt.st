testing
testAddMethodInExtensionCategoryNotExistingCreateANewPackageAndInstallsMethodInIt
	"test that when we add a method  in an extension category ( beginning with *)that does not refer to an existing categorya new package with the name of this category is added, and that the method is added to this new package"
	| class firstPackage t|
	
	self addXCategory.
	
	firstPackage := self organizer packageNamed: #XXXXX.
	
	class := self createNewClassNamed: 'NewClass' inCategory: 'XXXXX'.
	
	self createMethodNamed: #newMethod inClass:  class inCategory: '*SomethingDifferentNothingToDoWithWhatWeHave'.
	
	self deny: (class package includesDefinedSelector: #newMethod ofClass: class).
	
	self assert: (self organizer includesPackageNamed: #SomethingDifferentNothingToDoWithWhatWeHave).
	self assert: ((class >> #newMethod) packageFromOrganizer: self organizer ) equals: (self organizer packageNamed: #SomethingDifferentNothingToDoWithWhatWeHave).
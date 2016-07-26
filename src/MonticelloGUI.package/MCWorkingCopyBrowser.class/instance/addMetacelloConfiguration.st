actions
addMetacelloConfiguration
	|name|
	name := UIManager default 
				request: 'Name of the new Metacello configuration (e.g., ConfigurationOfYourSoftware)' translated 
				initialAnswer: 'ConfigurationOf'. 
	name isEmptyOrNil ifFalse:
		[ 
			"Check if the class does not exist"
			(Smalltalk globals includesKey: name asSymbol) ifTrue: [ self error: 'Class already exist'].

			"Check if metacello is loaded"
			(Smalltalk globals includesKey: #MetacelloConfigTemplate) ifFalse: 
				[Gofer new
  					squeaksource: 'MetacelloRepository';
  					package: 'ConfigurationOfMetacello';
 					 load.
				(Smalltalk at: #ConfigurationOfMetacello) perform: #loadLatestVersion].

			"Create the configuration"
			((Smalltalk globals at: #MetacelloConfigTemplate) duplicateClassWithNewName: name asSymbol)
        			category: name asString.

			"We create the package that has the same name"
			RPackageOrganizer default registerPackageNamed: name.
			
			"Select the package"
			workingCopy := MCWorkingCopy forPackage: (MCPackage new name: name).
			repository ifNotNil: [ workingCopy repositoryGroup addRepository: repository ].
			workingCopyWrapper := nil.
			workingCopy modified: true.
			self workingCopySelection: 0.
			self repositorySelection: 0 ].

	self workingCopyListChanged; 
		changed: #workingCopySelection;
		repositoryListChanged.
	self changedButtons.
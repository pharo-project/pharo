"
A SycAddNewPackageCommandTest is a test class for testing the behavior of SycAddNewPackageCommand
"
Class {
	#name : 'SycAddNewPackageCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-PackageCommands-Tests',
	#package : 'SystemCommands-PackageCommands-Tests'
}

{ #category : 'tests' }
SycAddNewPackageCommandTest >> testExecute [

	| systemEnvironment command |
	systemEnvironment := MockObject new
		                     on: #createPackageNamed:
		                     with: 'Test-Package'.

	command := SycAddNewPackageCommand new
		           systemEnvironment: systemEnvironment;
		           packageName: 'Test-Package';
		           execute.

	self assert: command resultPackage equals: systemEnvironment.
	self verify: systemEnvironment

]

{ #category : 'tests' }
SycAddNewPackageCommandTest >> testExecuteAlreadyExists [

	| systemEnvironment command |
	systemEnvironment := MockObject new.
	systemEnvironment
		on: #createPackageNamed:
		with: 'Test-Package'
		verify: [ 
			PackageConflictError signal:
				'A package named Test-Package already exists' ].
	systemEnvironment
		on: #packageNamed:
		with: 'Test-Package'
		respond: #sentinel.

	command := SycAddNewPackageCommand new
		           systemEnvironment: systemEnvironment;
		           packageName: 'Test-Package';
		           execute.

	self assert: command resultPackage equals: #sentinel.
	self verify: systemEnvironment
]

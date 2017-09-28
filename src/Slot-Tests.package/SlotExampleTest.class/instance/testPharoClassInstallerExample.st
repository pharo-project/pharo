tests
testPharoClassInstallerExample
	| newClass |
	[ 
		newClass := Smalltalk classInstaller example.
		self assert: newClass isBehavior.
	] ensure: [ 
		newClass removeFromSystem ].
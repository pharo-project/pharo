tests
testPharoClassInstallerExample
	| newClass |
	[ 
		newClass := PharoClassInstaller example.
		self assert: newClass isBehavior.
	] ensure: [ 
		newClass removeFromSystem ].
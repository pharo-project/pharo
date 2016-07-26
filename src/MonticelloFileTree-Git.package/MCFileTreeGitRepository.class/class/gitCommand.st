accessing
gitCommand
	"Preferences for Squeak as well."
	
	"Platform determination is probably Pharo-specific."

	<preference: 'Git Command Path'
		category: 'monticello'
		description: 'The complete path for the git command, if not in the default PATH and different in name'
		type: #String>

	^ GitCommand ifNil: [ Smalltalk os isWindows ifTrue: [ 'git' ] ifFalse:[ '/usr/bin/git' ] ]
	

private
runPackages
	| packages results |
	
	packages := self packages.
	self inform: 'Running tests in ', packages size asString, ' Packages'.
	results := (self testRunner runPackages: self packages) select: #notNil.
				
	self informResults: results.
	(self hasOption: 'save') ifTrue: [ Smalltalk saveSession ].
	((self hasOption: 'fail-on-failure') 
		and: [ results anySatisfy: #hasFailuresOrErrors ])
		ifTrue: [^ self exitFailure: 'Tests do not pass!' ].
	((self hasOption: 'fail-on-error') 
		and: [ results anySatisfy: #hasErrors ])
		ifTrue: [^ self exitFailure: 'Tests do not pass!' ].
	
	Smalltalk snapshot: false andQuit: true.
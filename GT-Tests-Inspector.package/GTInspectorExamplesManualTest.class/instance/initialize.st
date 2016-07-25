initialization
initialize
	| fileWithAllClasses fileWithAllTestedClasses |
	super initialize.
		
	" prepare all classes "
	allClasses := {}.
	fileWithAllClasses := FileSystem workingDirectory / 'gtExamplesAllClasses.txt'.
	fileWithAllClasses exists ifTrue: [ 
		allClasses := self readClassesFromFile: fileWithAllClasses ].
	allClasses isEmptyOrNil ifTrue: [
		| streamWithAllClasses |
		allClasses := SystemNavigation default allClassesAndTraits sort: [ :a :b | a name <= b name ].
		streamWithAllClasses := fileWithAllClasses writeStream.
		allClasses do: [ :each | streamWithAllClasses nextPutAll: each name; cr. ].
		streamWithAllClasses flush; close. ].

	" prepare tested classes "
	allTestedClasses := {}.
	fileWithAllTestedClasses := FileSystem workingDirectory / 'gtExamplesAllTestedClasses.txt'.
	fileWithAllTestedClasses exists ifTrue: [ 
		allTestedClasses := self readClassesFromFile: fileWithAllTestedClasses ].
	
	" prepare untested classes "
	allUntestedClasses := (allClasses difference: allTestedClasses) sort: [ :a :b | a name <= b name ].
	
	" prepare failures "
	allFailures := self readFailuresFromFile: (FileSystem workingDirectory / 'gtExampleAllFailures.txt').
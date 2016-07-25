private
createNewClassNamed: aName
	
	| cls |
	cls := Object subclass: aName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'RPackageTest'.
	^ cls
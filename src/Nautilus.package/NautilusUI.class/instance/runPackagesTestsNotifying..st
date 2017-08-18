private
runPackagesTestsNotifying: aBoolean
	| packages classes label |

	(packages := self selectedPackages) ifNil: [ ^ self ].
	label := 
	packages size > 1
		ifTrue: [ 'Packages: ' ]
		ifFalse: [ 'Package: ' ].
		
	classes := packages flatCollect: [ :package | package asNautilusSelection definedClasses ].
	classes := classes select: [ :class | class isTestCase ].
	
	self runClassTests: classes notifying: false.

	aBoolean ifTrue: [ self notifyTestResultFrom: packages classes: classes labeled: label ].
private
runPackagesTestsNotifying: aBoolean
	self selectedPackages
		ifNotNil: [ :packages | 
			| classes label |
			label := packages size > 1
				ifTrue: [ 'Packages: ' ]
				ifFalse: [ 'Package: ' ].
			classes := packages
				flatCollect: [ :package | package definedClasses ].
			classes := classes select: [ :class | class isTestCase ].
			self runClassTests: classes notifying: false.
			aBoolean
				ifTrue:
					[ self notifyTestResultFrom: packages classes: classes labeled: label ] ]
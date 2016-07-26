private
runPackagesTestsNotifying: aBoolean

	self selectedPackages
		ifNotNil: [:packages || classes label |
			label := 
			packages size > 1
				ifTrue: [ 'Packages: ' ]
				ifFalse: [ 'Package: ' ].
			classes := packages flatCollect: [:package | package definedClasses ].
			classes := classes select: [:class | class isTestCase ].
			self runClassTests: classes notifying: false.
			aBoolean
				ifTrue: [ 
					| color |
					color := Color gray.
					(classes anySatisfy:[:e  | e hasPassedTest ]) ifTrue: [ color := Color green ].
					(classes anySatisfy:[:e  | e hasFailedTest ]) ifTrue: [ color := Color yellow ].
					(classes anySatisfy:[:e  | e hasErrorTest ]) ifTrue: [ color := Color red ].
					self 
						notifyTitle: 'Test Finished'
						contents: label , ((packages collect: #name) joinUsing: ', ')
						color: color ]].
*Tool-Base
browseAllSendersOf: aLiteral localTo: aClass
	"Create and schedule a message browser on each method in or below the given class that refers to
	aLiteral.
	Example: 
		SystemNavigation new browseAllSendersOf: #open:label: localTo: CodeHolder."

	aClass ifNil: [ ^self inform: 'no selected class' ].
	^ self headingAndAutoselectForLiteral: aLiteral do:
		[:label :autoSelect|
		self browseMessageList: (aClass allLocalCallsOn: aLiteral) asSortedCollection
			name: label, ' local to ', aClass name
			autoSelect: autoSelect]
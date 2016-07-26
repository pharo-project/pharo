menus behavior
generateTestMethodsAndFocus: aBoolean
	"generate testing methods for the selected methods in the test class"

	"I assume the test class is named #{MyClassName}Test"

	| focus |
	self selectedMethods ifEmpty: [ ^ self ].
	self selectedMethods reverse
		do: [ :method | focus := self generateTestMethodFor: method ].
	focus ifNil: [ ^ self ].
	(aBoolean and: [ self okToChange ])
		ifTrue:
			[ 
			self selectedPackage: (self parentOfClass: focus methodClass).
			self selectedClass: focus methodClass.
			self selectedCategory: 'tests'.
			methodWidget selectMethod: focus.
			self selectedMethod: focus.
			self updatePackageGroupAndClassList ]
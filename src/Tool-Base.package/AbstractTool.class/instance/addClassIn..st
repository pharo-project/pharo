class
addClassIn: aPackage
	" Add a new class in the provided package. A dialog will be opened to let the user set the class definition "

	| classSymbol systemCategory proposedSuperclass classDefinition newClass  |
	classSymbol := #NameOfSubclass.
	systemCategory := aPackage
		ifNotNil: [ aPackage categoryName ]
		ifNil: [ 'Unknown' ].
	proposedSuperclass := self selectedClasses in: [ :classes |
		classes size == 1
			ifFalse: [ Object ]
			ifTrue: [ classes first ]].
	classDefinition := self class defineClassTemplate 
		format: { proposedSuperclass. classSymbol. systemCategory }.
	classDefinition := UIManager default 
		multiLineRequest: 'Define class definition:'
		initialAnswer: classDefinition
		answerHeight: 250.
	classDefinition isEmptyOrNil 
		ifTrue: [ ^ self ].
		
	newClass := self compileANewClassFrom: classDefinition notifying: nil startingFrom: nil.
	newClass ifNotNil: [self browseClass: newClass].
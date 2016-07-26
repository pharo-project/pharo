source code area
defaultClassDescriptor
	| string |
	string := Slot showSlotClassDefinition
		ifTrue: [ 
			'Object subclass: #NameOfSubclass
	slots: {}
	classVariables: {}
	category: ''' ]
		ifFalse: [ 
			'Object subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''' ].
	^ self selectedPackage ifNotNil: [ :package | string , package categoryName , '''' ] ifNil: [ string , '''' ]
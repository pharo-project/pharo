source code area
defaultClassDescriptor
	| string |
	string := (String new: 12) writeStream.
	string nextPutAll: 
'Object subclass: #NameOfSubClass
	uses: {}
'.
	Slot showSlotClassDefinition 
		ifTrue: [
			string nextPutAll:
'		slots: {}
		classVariables: {}
		category: ''']
		ifFalse: [
			string nextPutAll:
'	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''' ].
	self selectedPackage
		ifNil: [string nextPutAll: '''' ]
		ifNotNil: [:package | string nextPutAll: package name; nextPutAll: '''' ].
	^string contents
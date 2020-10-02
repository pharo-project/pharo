Extension { #name : #ClyClassCreationToolMorph }

{ #category : #'*FluidClassDefinition' }
ClyClassCreationToolMorph >> classTemplate [

	| template |
	
	template := Slot showSlotClassDefinition
		ifTrue: [ 
			^ ClassDescription definitionFullyExpandedButPackage: self packageName ].
	 
template := 'Object subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: '''.

	^ template , self packageName , '''' 
]

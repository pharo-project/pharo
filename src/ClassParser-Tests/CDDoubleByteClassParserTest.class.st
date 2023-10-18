Class {
	#name : 'CDDoubleByteClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDDoubleByteClassParserTest >> classDefinitionString [

	^ '{superclassName} << #{classname}
		layout: DoubleByteLayout;
		slots: {slots};
		sharedVariables: \{ #{classvar1} . #{classvar2} };
		package: #MyPackage' format: {
			  ('classname' -> self className).
			  ('superclassName' -> self superclassName).
			  ('slots' -> self slotsArray).
			  ('classvar1' -> self firstClassVariableName).
			  ('classvar2' -> self secondClassVariableName) } asDictionary
]

{ #category : 'helpers' }
CDDoubleByteClassParserTest >> testDoubleByteClass [

	self assert: classDefinition layoutClass equals: DoubleByteLayout
]

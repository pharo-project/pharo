Class {
	#name : 'CDVariableClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDVariableClassParserTest >> classDefinitionString [

	^ '{superclassName} variableSubclass: #{classname}
		slots: {slots};
		classVariableNames: ''{classvar1} {classvar2}''
		package: #MyPackage'
			format: {
				'classname' -> self className.
				'superclassName' -> self superclassName.
				'slots' -> self slotsArray.
				'classvar1' -> self firstClassVariableName.
				'classvar2' -> self secondClassVariableName. } asDictionary
]

{ #category : 'helpers' }
CDVariableClassParserTest >> testVariableClassIsVariable [

	self assert: classDefinition layoutClass equals: VariableLayout
]

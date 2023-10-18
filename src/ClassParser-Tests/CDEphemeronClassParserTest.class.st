Class {
	#name : 'CDEphemeronClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDEphemeronClassParserTest >> classDefinitionString [

	^ '{superclassName} ephemeronSubclass: #{classname}
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
CDEphemeronClassParserTest >> testEphemeronClassIsEphemeron [

	self assert: classDefinition layoutClass equals: EphemeronLayout
]

Class {
	#name : 'CDNormalClassCategoryParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDNormalClassCategoryParserTest >> classDefinitionString [

	^ '{superclassName} subclass: #{classname}
		slots: {slots};
		classVariableNames: ''{classvar1} {classvar2}''
		category: #MyPackage'
			format: {
				'classname' -> self className.
				'superclassName' -> self superclassName.
				'slots' -> self slotsArray.
				'classvar1' -> self firstClassVariableName.
				'classvar2' -> self secondClassVariableName. } asDictionary
]

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
		sharedVariables: {sharedVariables};
		category: #MyPackage'
			format: {
				'classname' -> self className.
				'superclassName' -> self superclassName.
				'slots' -> self slotsArray.
				'sharedVariables' -> self sharedVariablesArray } asDictionary
]

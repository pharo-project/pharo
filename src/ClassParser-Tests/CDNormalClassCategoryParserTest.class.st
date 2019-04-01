Class {
	#name : #CDNormalClassCategoryParserTest,
	#superclass : #CDClassDefinitionParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #helpers }
CDNormalClassCategoryParserTest >> classDefinitionString [

	^ '{superclassName} subclass: #{classname}
		instanceVariableNames: ''{instvar1} {instvar2}''
		classVariableNames: ''{classvar1} {classvar2}''
		category: #MyPackage'
			format: { 
				'classname' -> self className.
				'superclassName' -> self superclassName.
				'instvar1' -> self firstInstanceVariableName.
				'instvar2' -> self secondInstanceVariableName.
				'classvar1' -> self firstClassVariableName.
				'classvar2' -> self secondClassVariableName. } asDictionary
]

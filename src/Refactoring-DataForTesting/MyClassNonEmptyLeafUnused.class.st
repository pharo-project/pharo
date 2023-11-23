Class {
	#name : 'MyClassNonEmptyLeafUnused',
	#superclass : 'MyClassA',
	#instVars : [
		'instVarName1222',
		'instVarName23333'
	],
	#classVars : [
		'ClassVarName2456'
	],
	#category : 'Refactoring-DataForTesting-StaticModel',
	#package : 'Refactoring-DataForTesting',
	#tag : 'StaticModel'
}

{ #category : 'initialization' }
MyClassNonEmptyLeafUnused >> initialize [

	<ignoreUnusedVariables: #( instVarName1222 #instVarName23333 )>
	<ignoreUnusedClassVariables: #( ClassVarName2456 )>
	super initialize
]

{ #category : 'dummy methods' }
MyClassNonEmptyLeafUnused >> methodForThePleasureOfIt [

	^ 666
]

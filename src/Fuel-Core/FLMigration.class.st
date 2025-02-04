"
I represent migration information about a class.

See 'configuring-migration' protocol in FLMaterializer.
"
Class {
	#name : 'FLMigration',
	#superclass : 'Object',
	#instVars : [
		'sourceClassName',
		'variables',
		'targetClass'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
FLMigration class >> fromClassNamed: aSymbol toClass: aClass variables: anArray [ 

	^self basicNew
		initializeClassNamed: aSymbol 
		toClass: aClass 
		variables: anArray;
		yourself.
]

{ #category : 'evaluating' }
FLMigration >> applyTo: aVariablesMapping [ 
	
	variables associationsDo: [:link |
		aVariablesMapping 
			map: link key 
			to: link value ]
]

{ #category : 'initialization' }
FLMigration >> initializeClassNamed: aSymbol toClass: aClass variables: anArray [ 
	
	self initialize.
	sourceClassName := aSymbol.
	targetClass := aClass.
	variables := anArray.
]

{ #category : 'accessing' }
FLMigration >> sourceClassName [

	^ sourceClassName
]

{ #category : 'accessing' }
FLMigration >> targetClass [

	^ targetClass
]

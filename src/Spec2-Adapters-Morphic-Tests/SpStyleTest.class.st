Class {
	#name : #SpStyleTest,
	#superclass : #TestCase,
	#instVars : [
		'aClass'
	],
	#category : #'Spec2-Adapters-Morphic-Tests'
}

{ #category : #running }
SpStyleTest >> tearDown [ 

	aClass ifNotNil: [ :e | e removeFromSystem ].
	super tearDown
]

{ #category : #tests }
SpStyleTest >> testParsingAStyleIsNotAffectedByExistingClass [

	| aStylesheet |

	aClass := self class classInstaller make: [ :aClassBuilder | aClassBuilder name: #Font ].	
	
	aStylesheet := SpStyle createDefaultStyleSheet.
	
	self assert: aStylesheet styles isNotEmpty
	
	
]

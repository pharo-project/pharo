Class {
	#name : 'ReSpecShouldNotReferenceMorphicThemeRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#instVars : [
		'classFactory'
	],
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'running' }
ReSpecShouldNotReferenceMorphicThemeRuleTest >> setUp [

	super setUp.
	classFactory := ClassFactoryForTestCase environment: SystemEnvironment new
]

{ #category : 'running' }
ReSpecShouldNotReferenceMorphicThemeRuleTest >> tearDown [

	classFactory cleanUp.
	super tearDown
]

{ #category : 'tests' }
ReSpecShouldNotReferenceMorphicThemeRuleTest >> testRule [

	| class |
	"Install the global so the following code compiles"
	classFactory environment at: #Smalltalk put: nil.
	class := classFactory make: [ :builder | builder superclass: StPresenter ].

	class compile: 'method ^ Smalltalk ui theme textColor' classified: 'initialization'.

	self assert: (self myCritiquesOnMethod: class >> #method) size equals: 1
]

{ #category : 'tests' }
ReSpecShouldNotReferenceMorphicThemeRuleTest >> testRuleNotViolated [

	self generateMethod: 'method ^ StandardFonts codeFont'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #method)
]

{ #category : 'tests' }
ReSpecShouldNotReferenceMorphicThemeRuleTest >> testRuleReportsSpPresenterSubclasses [

	| class |
	classFactory environment at: #Smalltalk put: nil.
	class := classFactory make: [ :builder | builder superclass: SpPresenter ].

	class compile: 'method ^ Smalltalk ui theme textColor' classified: 'initialization'.

	self assert: (self myCritiquesOnMethod: class >> #method) size equals: 1
]

{ #category : 'tests' }
ReSpecShouldNotReferenceMorphicThemeRuleTest >> testRuleSelfThemeIsReported [

	| class |
	classFactory environment at: #Smalltalk put: nil.
	class := classFactory make: [ :builder | builder superclass: StPresenter ].

	class compile: 'method ^ self theme textColor' classified: 'initialization'.

	self assert: (self myCritiquesOnMethod: class >> #method) size equals: 1
]

Class {
	#name : 'ReSafeMethodNameForbasedOnTest',
	#superclass : 'TestCase',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Tests-Model',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Model'
}

{ #category : 'tests' }
ReSafeMethodNameForbasedOnTest >> model [

	^ model ifNil: [ model := RBNamespace onEnvironment: (RBClassEnvironment classes: {
				   MyClassBetaSubSub .
				   MyClassBetaSub.
				   MyClassBeta.
				   MyClassAlpha.
					Number.
					Integer
					 })]
]

{ #category : 'tests' }
ReSafeMethodNameForbasedOnTest >> testBinaryMessages [

	| myClassNumber myClassInteger tempSymbol |
	
	myClassNumber := self model classNamed: #Number.
	myClassInteger := self model classNamed: #Integer.
	
	"Test for existing method name in class, bianry messages requiure no appendeding to be safe."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassInteger basedOn: '+'.
	self assert: tempSymbol equals: '+' asSymbol.
	
	"Test for existing method name in subclass, bianry messages requiure no appendeding to be safe."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassNumber basedOn: '\\\'.
	self assert: tempSymbol equals: '\\\' asSymbol.
	
	"Test for existing method name in superclass, bianry messages requiure no appendeding to be safe"
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassInteger basedOn: '**'.
	self assert: tempSymbol equals: '**' asSymbol.
	
	"Test for non-existing method name, bianry messages requiure no appendeding to be safee."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassInteger basedOn: '$%$'.
	self assert: tempSymbol equals: '$%$' asSymbol.
	

]

{ #category : 'tests - parameters' }
ReSafeMethodNameForbasedOnTest >> testBuildParametersForSelectorForBinary [

	| parameters |
	parameters := ReAbstractTransformation new buildParamtersForSelector: #+.
	self assert: parameters asArray equals: #('param1').
	
	
]

{ #category : 'tests - parameters' }
ReSafeMethodNameForbasedOnTest >> testBuildParametersForSelectorForKeywords [

	| parameters |
	parameters := ReAbstractTransformation new buildParamtersForSelector: #foo:bar:.
	self assert: parameters asArray equals: #('param1' 'param2').
	
	
]

{ #category : 'tests - parameters' }
ReSafeMethodNameForbasedOnTest >> testBuildParametersForSelectorForUnary [

	| parameters |
	parameters := ReAbstractTransformation new buildParamtersForSelector: #class.
	self assert: parameters asArray equals: #().
	
	
]

{ #category : 'tests' }
ReSafeMethodNameForbasedOnTest >> testKeywordMessages [

	| myClassAlpha myClassBeta myClassBetaSub myClassBetaSubSub tempSymbol |
	
	myClassAlpha := self model classNamed: #MyClassAlpha.
	myClassBeta := self model classNamed: #MyClassBeta.
	myClassBetaSub := self model classNamed: #MyClassBetaSub.
	myClassBetaSubSub := self model classNamed: #MyClassBetaSubSub.
	
	"Test for existing method name in class, requiures an appended '1'."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassBeta basedOn: 'fooAccessor:'.
	self assert: tempSymbol equals: #fooAccessor1:.
	
	"Test for existing method name in subclass, requiures an appended '1'."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassAlpha basedOn: 'fooAccessor:'.
	self assert: tempSymbol equals: #fooAccessor1:.
	
	"Test for existing method name in superclass, requiures an appended '1'."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassBetaSub basedOn: 'fooAccessor:'.
	self assert: tempSymbol equals: #fooAccessor1:.
	
	"Test for non-existing method name, requiures no appendeding to be safe."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassBeta basedOn: 'notAMethodAnywhere:'.
	self assert: tempSymbol equals: #notAMethodAnywhere:.
	

]

{ #category : 'tests' }
ReSafeMethodNameForbasedOnTest >> testUnaryMessages [

	| myClassBeta myClassBetaSub myClassBetaSubSub tempSymbol |
	
	myClassBeta := self model classNamed: #MyClassBeta.
	myClassBetaSub := self model classNamed: #MyClassBetaSub.
	myClassBetaSubSub := self model classNamed: #MyClassBetaSubSub.
	
	"Test for existing method name in class, requiures an appended '1'."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassBeta basedOn: 'fooAccessor'.
	self assert: tempSymbol equals: #fooAccessor1.
	
	"Test for existing method name in subclass, requiures an appended '1'."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassBeta basedOn: 'methodDuplicatedInSubclass3'.
	self assert: tempSymbol equals: #methodDuplicatedInSubclass31.
	
	"Test for existing method name in superclass, requiures an appended '1'."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassBetaSub basedOn: 'methodOverriden'.
	self assert: tempSymbol equals: #methodOverriden1.
	
	"Test for non-existing method name, requiures no appendeding to be safe."
	tempSymbol := ReAbstractTransformation new safeMethodNameFor: myClassBeta basedOn: 'notAMethodAnywhere'.
	self assert: tempSymbol equals: #notAMethodAnywhere.
	

]

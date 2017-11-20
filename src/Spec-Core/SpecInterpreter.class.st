"
A SpecInterpreter is a interpreter for spec presenters
"
Class {
	#name : #SpecInterpreter,
	#superclass : #Object,
	#instVars : [
		'model',
		'spec',
		'arrayToInterpret',
		'index'
	],
	#classInstVars : [
		'bindings'
	],
	#category : #Spec-Core
}

{ #category : #accessing }
SpecInterpreter class >> bindings [

	^ self defaultBindings 
]

{ #category : #accessing }
SpecInterpreter class >> bindings: anAdapterBinding [

	bindings := anAdapterBinding
]

{ #category : #private }
SpecInterpreter class >> defaultBindings [
	
	^ (CurrentSpecDefaultBindings value ifNil: [ self environment at: #MorphicAdapterBindings]) new
	
]

{ #category : #accessing }
SpecInterpreter class >> hardResetBindings [

	bindings := nil
]

{ #category : #protocol }
SpecInterpreter class >> interpretASpec: aSpec model: aModel [
	
	^ self
		interpretASpec: aSpec
		model: aModel
		selector: nil
]

{ #category : #protocol }
SpecInterpreter class >> interpretASpec: aPresenter model: aModel selector: aSelector [
	| result |
	
	 [ result :=  self 
				private_interpretASpec: aPresenter
				model: aModel
				selector: aSelector ] ensure: [ self hardResetBindings ].
	^ result
]

{ #category : #private }
SpecInterpreter class >> private_buildWidgetFor: aComposablePresenter withSpec: aSymbol [
	
	^ self private_interpretASpec: (aComposablePresenter retrieveSpec: aSymbol) model: aComposablePresenter selector: aSymbol
]

{ #category : #private }
SpecInterpreter class >> private_interpretASpec: aPresenter model: aModel selector: aSelector [
	
	^ self new
		interpretASpec: aPresenter
		model: aModel
		selector: aSelector
]

{ #category : #interpreting-private }
SpecInterpreter >> actionToPerformWithSelector: selector arguments: args [

	^ spec instance perform: selector withArguments: args
]

{ #category : #bindings }
SpecInterpreter >> bindings [

	^ self class bindings
]

{ #category : #interpreting-private }
SpecInterpreter >> computeSpecFrom: aSymbol selector: aSelector [
	| instance |

	instance := (aSymbol isSymbol and: [ aSymbol ~= #model ])
		ifTrue: [ | result |
			result := self convertSymbolOfClassToInstance: aSymbol.
			result isSpecContainer
				ifTrue: [ 
					result := result buildAdapterWithSpec.
					model addDependent: result ].
			result ]
		ifFalse: [ self class private_interpretASpec: aSymbol model: model selector: aSelector ].
		
	^ SpecWrapper
		instance: instance
		selector: aSelector
]

{ #category : #bindings }
SpecInterpreter >> convertSymbolOfClassToInstance: aSymbol [
	| translatedSymbol |
	
	translatedSymbol := self bindings translateSymbol: aSymbol.
	
	^ (Smalltalk 
		at: translatedSymbol
		ifAbsent: [ ^ translatedSymbol ]) new
]

{ #category : #interpreting-private }
SpecInterpreter >> extractArrayToInterpretFrom: aFragment [

	arrayToInterpret := aFragment.
	
	aFragment isSpecLayout
		ifTrue: [ arrayToInterpret := aFragment asArray ].
	
	(arrayToInterpret isCollection and: [ arrayToInterpret isSequenceable ])
		ifFalse: [ ^ arrayToInterpret ].
	
	(aFragment isString or: [ aFragment isText ])
		ifTrue: [ 
			^ aFragment == #model
				ifTrue: [ model ]
				ifFalse: [ self convertSymbolOfClassToInstance: aFragment ] ].
	^ nil
]

{ #category : #interpreting }
SpecInterpreter >> interpretASpec: aPresenter model: aModel  selector: aSelector [
	
	self model: aModel.
	^ self interpretASpec: aPresenter selector: aSelector
]

{ #category : #interpreting }
SpecInterpreter >> interpretASpec: aPresenter selector: aSelector [
	aPresenter ifNil: [ ^ nil ].
	
	(self extractArrayToInterpretFrom: aPresenter) ifNotNil: [ :result | ^ result ].
	
	(self retrieveSpecFrom: aPresenter selector: aSelector)
		ifNotNil: [ :instance | ^ instance ].

	index := 2.
	[ index <= arrayToInterpret size ]
		whileTrue: [ 
			| newInstance |
			
			newInstance := self performNextSelectorAndIncrementIndex.
			
			index > arrayToInterpret size
				ifTrue: [ ^ self returnInterpretationOf: newInstance ].
			spec instance: newInstance ].	
			
	"If you get here, there is a problem. Must probably it comes from the Spec which is wrongly defined"
	self assert: arrayToInterpret size < 2.
	
	(model respondsTo: #spec:)
		ifTrue: [ model spec: spec ].
		
	^ spec instance
]

{ #category : #accessing }
SpecInterpreter >> model [
	^ model
]

{ #category : #accessing }
SpecInterpreter >> model: anObject [
	model := anObject
]

{ #category : #interpreting-private }
SpecInterpreter >> performNextSelectorAndIncrementIndex [
	| args numArgs selector |

	selector := (arrayToInterpret at: index).
	selector isArray not
		ifTrue: [ 
			selector := selector asSymbol.
			numArgs := selector numArgs.
			args := arrayToInterpret copyFrom: index + 1 to: index + numArgs ]
		ifFalse: [ 
			"Here I assume that if it's not a symbol, it's a collection"
			| array |
			array := selector.
			selector := array first.
			numArgs := 0.
			args := array allButFirst ].
	
	args := args collect: [ :each | self class private_interpretASpec: each model: model selector: spec selector ].

	index := index + numArgs + 1.
	^ self actionToPerformWithSelector: selector arguments: args
]

{ #category : #interpreting-private }
SpecInterpreter >> retrieveSpecFrom: aPresenter selector: aSelector [

	((self model respondsTo: #needRebuild) and: [ self model needRebuild not and: [ self model spec notNil ] ])
		ifTrue: [
			spec := self model spec.
			self model needRebuild: true.
			((spec respondsTo: #isRedrawable) and: [ spec instance isSpecAdapter ])
				ifFalse: [ spec := self computeSpecFrom: arrayToInterpret first selector: aSelector ]
				ifTrue: [ 
					spec isRedrawable
						ifTrue: [ spec removeSubWidgets ]
						ifFalse: [ ^ spec instance ] ] ]
		ifFalse: [ spec := self computeSpecFrom: arrayToInterpret first selector: aSelector ].
	
	aSelector
		ifNil: [ aPresenter isSpecLayout ifTrue: [ spec selector: aPresenter selector ] ]
		ifNotNil: [ spec selector: aSelector ].
		
	^ nil
]

{ #category : #interpreting-private }
SpecInterpreter >> returnInterpretationOf: newInstance [
	| result |
	(model respondsTo: #spec:)
		ifTrue: [ model spec: spec ].
	result := self class private_interpretASpec: newInstance model: spec instance selector: spec selector.
	^ (result isKindOf: ComposablePresenter)
		ifTrue: [ 
			| return |
			return := result private_buildWithSpec.
			^ return ]
		ifFalse: [ ^ result ]
]

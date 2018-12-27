"
A SpecInterpreter is a interpreter for spec presenters
"
Class {
	#name : #SpecInterpreter,
	#superclass : #Object,
	#instVars : [
		'spec',
		'arrayToInterpret',
		'index',
		'presenter'
	],
	#classInstVars : [
		'bindings'
	],
	#category : #'Spec-Core-Utilities'
}

{ #category : #accessing }
SpecInterpreter class >> bindings [
	"Spec bindings is a DynamicVariable. It has a default value. In order to change it you can do something like: 
	
	SpecBindings value: MorphicAdapterBindings during: [ myUI openWithSpec ].
	
	"
	
	^ SpecBindings value new
]

{ #category : #protocol }
SpecInterpreter class >> interpretASpec: aSpec presenter: aPresenter [
	^ self new interpretASpec: aSpec presenter: aPresenter
]

{ #category : #'interpreting-private' }
SpecInterpreter >> actionToPerformWithSelector: selector arguments: args [
	^ [ spec perform: selector withArguments: args ]
		on: MessageNotUnderstood
		do: [ :ex | 
			"Maybe the message is not understood because we are trying to access a sub-part of a presenter but there is no accessor. In that case we can try to directly access the presenter.

			We are thinking of killing the interpreter. Maybe later if we know that we are getting the content of a variable of a presenter we can avoid using a catch and directly access the variable."
			(selector last ~= $: and: [ args isEmpty ])
				ifTrue: [ spec class instVarNames
						detect: [ :var | var = selector ]
						ifFound: [ :var | spec instVarNamed: selector ]
						ifNone: [ self error: spec asString , ' does not implement the method ' , selector , '. Maybe your spec reference a presenter that does not exists.' ] ]
				ifFalse: [ ex resume ] ]
]

{ #category : #bindings }
SpecInterpreter >> bindings [

	^ self class bindings
]

{ #category : #'interpreting-private' }
SpecInterpreter >> computeSpecFrom: aSymbol [
	^ (aSymbol isSymbol and: [ (self isPresenterSymbol: aSymbol) not ])
		ifTrue: [ | result |
			result := self convertSymbolOfClassToInstance: aSymbol.
			result isSpecContainer
				ifTrue: [ result := self getAdaptorWithDefaultSpecOf: result.
					self presenter addDependent: result ].
			result ]
		ifFalse: [ self class interpretASpec: aSymbol presenter: self presenter ]
]

{ #category : #bindings }
SpecInterpreter >> convertSymbolOfClassToInstance: aSymbol [
	| translatedSymbol |
	
	translatedSymbol := self bindings translateSymbol: aSymbol.
	
	^ (Smalltalk 
		at: translatedSymbol
		ifAbsent: [ ^ translatedSymbol ]) new
]

{ #category : #'interpreting-private' }
SpecInterpreter >> extractArrayToInterpretFrom: aFragment [

	arrayToInterpret := aFragment.
	
	aFragment isSpecLayout
		ifTrue: [ arrayToInterpret := aFragment asArray ].
	
	(arrayToInterpret isCollection and: [ arrayToInterpret isSequenceable ])
		ifFalse: [ ^ arrayToInterpret ].
	
	(aFragment isString or: [ aFragment isText ])
		ifTrue: [ 
			^ (self isPresenterSymbol: aFragment)
				ifTrue: [ self presenter ]
				ifFalse: [ self convertSymbolOfClassToInstance: aFragment ] ].
	^ nil
]

{ #category : #'interpreting-private' }
SpecInterpreter >> getAdaptorWithDefaultSpecOf: result [
	^ result buildAdapterWithSpecLayout: result retrieveDefaultSpec
]

{ #category : #interpreting }
SpecInterpreter >> interpretASpec: aSpec [
	aSpec ifNil: [ ^ nil ].
	
	(self extractArrayToInterpretFrom: aSpec) ifNotNil: [ :result | ^ result ].
	
	(self retrieveSpecFrom: aSpec) ifNotNil: [ :instance | ^ instance ].

	index := 2.
	[ index <= arrayToInterpret size ]
		whileTrue: [ 
			| newInstance |
			
			newInstance := self performNextSelectorAndIncrementIndex.
			
			index > arrayToInterpret size
				ifTrue: [ ^ self returnInterpretationOf: newInstance ].
			spec := newInstance ].	
			
	"If you get here, there is a problem. Must probably it comes from the Spec which is wrongly defined"
	self assert: arrayToInterpret size < 2.
	
	self presenter spec: spec.

	^ spec
]

{ #category : #interpreting }
SpecInterpreter >> interpretASpec: aSpec presenter: aPresenter [
	self presenter: aPresenter.
	^ self interpretASpec: aSpec
]

{ #category : #testing }
SpecInterpreter >> isPresenterSymbol: aSymbol [
	self flag: #todo. "We keep #model for one version of Pharo then we should only keep #presenter."
	
	^ #(#model #presenter) anySatisfy: [ :symbol | symbol = aSymbol ]
]

{ #category : #'interpreting-private' }
SpecInterpreter >> performNextSelectorAndIncrementIndex [
	| args selector |
	selector := arrayToInterpret at: index.
	
	self
		assert: selector isArray not
		description: 'Before there was an #ifTrue:ifFalse: on `selector isArray` but it seems that the selector could never be an Array. If one day we have an array here, please report the problem and link it to https://github.com/pharo-spec/Spec/issues/60'.
		
	selector := selector asSymbol.
	args := (arrayToInterpret copyFrom: index + 1 to: index + selector numArgs) collect: [ :each | self class interpretASpec: each presenter: self presenter ].
	index := index + selector numArgs + 1.
	
	^ self actionToPerformWithSelector: selector arguments: args
]

{ #category : #accessing }
SpecInterpreter >> presenter [
	^ presenter
]

{ #category : #accessing }
SpecInterpreter >> presenter: aPresenter [
	presenter := aPresenter
]

{ #category : #'interpreting-private' }
SpecInterpreter >> retrieveSpecFrom: aSpec [
	self presenter needRebuild
		ifTrue: [ spec := self computeSpecFrom: arrayToInterpret first ]
		ifFalse: [ spec := self presenter spec.
			self presenter needRebuild: true.
			spec
				ifNil: [ spec := self computeSpecFrom: arrayToInterpret first ]
				ifNotNil: [ spec isRedrawable
						ifTrue: [ spec removeSubWidgets ]
						ifFalse: [ ^ spec ] ] ].
	^ nil
]

{ #category : #'interpreting-private' }
SpecInterpreter >> returnInterpretationOf: newInstance [
	| result |
	self presenter spec: spec.
	result := self class interpretASpec: newInstance presenter: spec.
	^ (result isKindOf: ComposablePresenter)
		ifTrue: [ self getAdaptorWithDefaultSpecOf: result ]
		ifFalse: [ result ]
]

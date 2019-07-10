"
A SpecInterpreter is a interpreter for spec presenters
"
Class {
	#name : #SpInterpreter,
	#superclass : #Object,
	#instVars : [
		'spec',
		'arrayToInterpret',
		'index',
		'presenter',
		'selector'
	],
	#category : #'Spec2-Core-Utilities'
}

{ #category : #accessing }
SpInterpreter class >> bindings [
	"Spec bindings is a DynamicVariable. It has a default value. In order to change it you can do something like: 
	
	SpecBindings value: MorphicAdapterBindings during: [ myUI openWithSpec ].
	
	"
	
	^ SpBindings value
]

{ #category : #protocol }
SpInterpreter class >> interpretASpec: aSpec presenter: aPresenter [
	^ self new interpretASpec: aSpec presenter: aPresenter
]

{ #category : #'interpreting-private' }
SpInterpreter >> actionToPerformWithSelector: aSelector arguments: args [
	^ [ spec perform: aSelector withArguments: args ]
		on: MessageNotUnderstood
		do: [ :ex | 
			"Maybe the message is not understood because we are trying to access a sub-part of a presenter but there is no accessor. In that case we can try to directly access the presenter.

			We are thinking of killing the interpreter. Maybe later if we know that we are getting the content of a variable of a presenter we can avoid using a catch and directly access the variable."
			(aSelector last ~= $: and: [ args isEmpty ])
				ifTrue: [ self subpresenterNamed: aSelector]
				ifFalse: [ ex pass ] ]
]

{ #category : #bindings }
SpInterpreter >> bindings [

	^ self class bindings
]

{ #category : #'interpreting-private' }
SpInterpreter >> computeSpecFrom: aSymbol [
	| res |
	res := (aSymbol isSymbol and: [ (self isPresenterSymbol: aSymbol) not ])
		ifTrue: [ | result |
			result := self convertSymbolOfClassToInstance: aSymbol.
			(result isKindOf: SpContainerPresenter) "I take the liberty to add this ugly code since we want to remove it and it simplify things for Spec 2 migration"
				ifTrue: [ result := self getAdaptorWithDefaultSpecOf: result.
					self presenter addDependent: result ].
			result ]
		ifFalse: [ self class interpretASpec: aSymbol presenter: self presenter ].

	selector ifNotNil: [ :sel | res selector: sel ].

	^ res
]

{ #category : #bindings }
SpInterpreter >> convertSymbolOfClassToInstance: aSymbol [
	| translatedSymbol |
	
	translatedSymbol := self bindings translateSymbol: aSymbol.
	
	^ (Smalltalk 
		at: translatedSymbol
		ifAbsent: [ ^ translatedSymbol ]) owner: self presenter
]

{ #category : #'interpreting-private' }
SpInterpreter >> extractArrayToInterpretFrom: aFragment [
	arrayToInterpret := aFragment.

	aFragment isSpLayout
		ifTrue: [ arrayToInterpret := aFragment asArray.
			selector := aFragment selector ].

	(arrayToInterpret isCollection and: [ arrayToInterpret isSequenceable ]) ifFalse: [ ^ arrayToInterpret ].

	(aFragment isString or: [ aFragment isText ])
		ifTrue: [ ^ (self isPresenterSymbol: aFragment) ifTrue: [ self presenter ] ifFalse: [ self convertSymbolOfClassToInstance: aFragment ] ].
	^ nil
]

{ #category : #'interpreting-private' }
SpInterpreter >> getAdaptorWithDefaultSpecOf: result [
	^ result buildAdapterWithSpecLayout: result retrieveDefaultSpec
]

{ #category : #interpreting }
SpInterpreter >> interpretASpec: aSpec [
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
SpInterpreter >> interpretASpec: aSpec presenter: aPresenter [
	self presenter: aPresenter.
	^ self interpretASpec: aSpec
]

{ #category : #testing }
SpInterpreter >> isPresenterSymbol: aSymbol [
	self flag: #todo. "We keep #model for one version of Pharo then we should only keep #presenter."
	
	^ #(#model #presenter) anySatisfy: [ :symbol | symbol = aSymbol ]
]

{ #category : #'interpreting-private' }
SpInterpreter >> performNextSelectorAndIncrementIndex [
	| args aSelector |
	aSelector := arrayToInterpret at: index.
	
	self
		assert: aSelector isArray not
		description: 'Before there was an #ifTrue:ifFalse: on `aSelector isArray` but it seems that the aSelector could never be an Array. If one day we have an array here, please report the problem and link it to https://github.com/pharo-spec/Spec/issues/60'.
		
	aSelector := aSelector asSymbol.
	args := (arrayToInterpret copyFrom: index + 1 to: index + aSelector numArgs) collect: [ :each | self class interpretASpec: each presenter: self presenter ].
	index := index + aSelector numArgs + 1.
	
	^ self actionToPerformWithSelector: aSelector arguments: args
]

{ #category : #accessing }
SpInterpreter >> presenter [
	^ presenter
]

{ #category : #accessing }
SpInterpreter >> presenter: aPresenter [
	presenter := aPresenter
]

{ #category : #'interpreting-private' }
SpInterpreter >> retrieveSpecFrom: aSpec [

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
SpInterpreter >> returnInterpretationOf: newInstance [
	| result adaptor |
	result := self class interpretASpec: newInstance presenter: spec.
	adaptor := (result isKindOf: SpPresenter)
		ifTrue: [ self getAdaptorWithDefaultSpecOf: result ]
		ifFalse: [ result ].
	self presenter spec: adaptor.
	^ adaptor
]

{ #category : #'interpreting-private' }
SpInterpreter >> subpresenterNamed: aSelector [

 	^ spec additionalSubpresentersMap at: aSelector ifAbsent: [
		spec class allInstVarNames
			detect: [ :var | var = aSelector ]
			ifFound: [ :var | spec instVarNamed: aSelector ]
			ifNone: [ self error: spec asString , ' does not implement the method ' , aSelector , '. Maybe your spec reference a presenter that does not exists.' ]
		]
]

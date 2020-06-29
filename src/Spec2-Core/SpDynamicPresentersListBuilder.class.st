"
I allow to build a list of spec presenters into a DynamicComposablePresenter depending on an arbitrary number of objects.

I also allow to specify the layout to use for these presenters.

See
[[[
	self example.
]]]
"
Class {
	#name : #SpDynamicPresentersListBuilder,
	#superclass : #Object,
	#instVars : [
		'configurationBlock',
		'instVarCount',
		'dynamicPresenter',
		'nullPresentersInstVarNames',
		'layoutBuilder',
		'instVarToModelObject',
		'presenterBlock'
	],
	#category : #'Spec2-Core-Dynamic-Builder'
}

{ #category : #example }
SpDynamicPresentersListBuilder class >> example [
	| dynamicPresenter |
	dynamicPresenter := SpDynamicPresentersListBuilder new
		modelObjects: {1 . 2 . 3 . 4 . 5};
		presenter: SpCheckBoxPresenter
			configuredAs: [ :presenter :modelObject | 
			presenter
				label: modelObject asString;
				whenActivatedDo: [ UIManager default inform: modelObject asString , ' activated.' ];
				whenDeactivatedDo: [ UIManager default inform: modelObject asString , ' deactivated.' ] ];
		layoutBuilder: (SpDynamicTopToBottomColumnsLayout columns: 2);
		build.
	
	^ dynamicPresenter
		openWithSpec;
		yourself
]

{ #category : #example }
SpDynamicPresentersListBuilder class >> example2 [
	| dynamicPresenter |
	dynamicPresenter := SpDynamicPresentersListBuilder new
		modelObjects: {1 . 2 . 3 . 4 . 5};
		presenterProvidedBy: [ :integer |
			integer odd "In your real code, avoid conditionals, use dispatch."
				ifTrue: [ SpCheckBoxPresenter ]
				ifFalse: [ SpLabelPresenter ] ]
			configuredAs: [ :presenter :modelObject | 
			presenter
				label: modelObject asString ];
		layoutBuilder: (SpDynamicTopToBottomColumnsLayout columns: 2);
		build.
	
	^ dynamicPresenter
		openWithSpec;
		yourself
]

{ #category : #building }
SpDynamicPresentersListBuilder >> build [
	^ self buildDynamicPresenter
		layout: self buildLayout;
		yourself
]

{ #category : #building }
SpDynamicPresentersListBuilder >> buildDynamicPresenter [
	dynamicPresenter := SpDynamicPresenter new.
	dynamicPresenter instantiatePresenters: self instVarNamesAndPresenterNamesArray.
	
	"Apply configuration block."
	self instVarToModelObjectDo: [ :instVarName :object |
		self configurationBlock value: (instVarName value: dynamicPresenter) value: object ].
	^ dynamicPresenter
]

{ #category : #building }
SpDynamicPresentersListBuilder >> buildLayout [
	^ self layoutBuilder buildFor: self
]

{ #category : #accessing }
SpDynamicPresentersListBuilder >> configurationBlock [
	^ configurationBlock
]

{ #category : #accessing }
SpDynamicPresentersListBuilder >> configurationBlock: anObject [
	configurationBlock := anObject
]

{ #category : #private }
SpDynamicPresentersListBuilder >> createNullPresenters: anInteger [
	nullPresentersInstVarNames := self generateNullPresentersInstVarNames: anInteger.

	dynamicPresenter
		instantiatePresenters: self nullPresentersInstVarNamesAndPresenterNamesArray
]

{ #category : #private }
SpDynamicPresentersListBuilder >> generateNullPresentersInstVarNames: anInteger [
	^ (1 to: anInteger)
		collect: [ :i | self generateUniqueInstanceVariableName ]
]

{ #category : #private }
SpDynamicPresentersListBuilder >> generateUniqueInstanceVariableName [
	instVarCount := instVarCount + 1.
	^ (#var , instVarCount asString) asSymbol
]

{ #category : #initialization }
SpDynamicPresentersListBuilder >> initialize [
	super initialize.
	instVarCount := 0.
	nullPresentersInstVarNames := #()
]

{ #category : #private }
SpDynamicPresentersListBuilder >> instVarNames [
	^ self instVarToModelObject collect: #key
]

{ #category : #private }
SpDynamicPresentersListBuilder >> instVarNamesAndPresenterNamesArray [
	^ (self instVarToModelObject
		inject: OrderedCollection new
		into: [ :col :instVarToModelObjectAssoc |
			col
				add: instVarToModelObjectAssoc key;
				add: (self presenterBlock value: instVarToModelObjectAssoc value) name;
				yourself ]) asArray
]

{ #category : #private }
SpDynamicPresentersListBuilder >> instVarToModelObject [
	^ instVarToModelObject
]

{ #category : #private }
SpDynamicPresentersListBuilder >> instVarToModelObjectDo: aBlock [
	"Iterates on inst. var. names and model object and provides them as argument to the aBlock."
	self instVarToModelObject do: [ :assoc |
		aBlock value: assoc key value: assoc value ]
]

{ #category : #accessing }
SpDynamicPresentersListBuilder >> layoutBuilder [
	^ layoutBuilder
]

{ #category : #accessing }
SpDynamicPresentersListBuilder >> layoutBuilder: anObject [
	layoutBuilder := anObject
]

{ #category : #accessing }
SpDynamicPresentersListBuilder >> modelObjects [
	^ instVarToModelObject collect: #value
]

{ #category : #accessing }
SpDynamicPresentersListBuilder >> modelObjects: aCollectionOfObjects [
	instVarToModelObject := aCollectionOfObjects collect: [ :o |
		self generateUniqueInstanceVariableName -> o ]
]

{ #category : #private }
SpDynamicPresentersListBuilder >> nullPresentersInstVarNames [
	^ nullPresentersInstVarNames
]

{ #category : #private }
SpDynamicPresentersListBuilder >> nullPresentersInstVarNamesAndPresenterNamesArray [
	^ (self nullPresentersInstVarNames
		inject: OrderedCollection new
		into: [ :col :instVar |
			col
				add: instVar;
				add: SpNullPresenter name;
				yourself ]) asArray
]

{ #category : #api }
SpDynamicPresentersListBuilder >> presenter: aPresenterClass configuredAs: twoArgumentsBlock [
	"Configure the builder: 1. provide the presenter class to instantiate; and 2. specify how to configure it.
	 - aPresenterClass is a the presenter class to instantiate for each model object.
	 - twoArgumentsBlock will be called to configure instances of aPresenterClass. The first argument of this block is the presenter instance and the second the model object."
	self presenterProvidedBy: [ :modelObject | aPresenterClass ] configuredAs: twoArgumentsBlock
]

{ #category : #accessing }
SpDynamicPresentersListBuilder >> presenterBlock [
	^ presenterBlock
]

{ #category : #accessing }
SpDynamicPresentersListBuilder >> presenterBlock: anObject [
	presenterBlock := anObject
]

{ #category : #api }
SpDynamicPresentersListBuilder >> presenterProvidedBy: blockReturningPresenter configuredAs: blockConfiguringThePresenter [
	"Configure the builder: 1. provide a block returning presenter classes to instantiate; and 2. specify how to configure them.
	 I do a job similar to #presenter:configuredAs: but I provide more flexibility since model objects can be shown as different presenters.
	 - blockReturningPresenter is a block returning the presenter class to instantiate for each model object. It has one argument which the model object that needs to be shown on the UI.
	 - twoArgumentsBlock will be called to configure instances of aPresenterClass. The first argument of this block is the presenter instance and the second the model object."
	self
		presenterBlock: blockReturningPresenter;
		configurationBlock: blockConfiguringThePresenter
	
]

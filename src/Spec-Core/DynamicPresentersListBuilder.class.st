"
I allow to build a list of spec presenters into a DynamicComposablePresenter depending on an arbitrary number of objects.

I also allow to specify the layout to use for these presenters.

See
[[[
	self example.
]]]
"
Class {
	#name : #DynamicPresentersListBuilder,
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
	#category : #'Spec-Core-Dynamic-Builder'
}

{ #category : #example }
DynamicPresentersListBuilder class >> example [
	| dynamicPresenter |
	dynamicPresenter := DynamicPresentersListBuilder new
		modelObjects: {1 . 2 . 3 . 4 . 5};
		presenter: CheckBoxPresenter
			configuredAs: [ :presenter :modelObject | 
			presenter
				label: modelObject asString;
				whenActivatedDo: [ UIManager default inform: modelObject asString , ' activated.' ];
				whenDeactivatedDo: [ UIManager default inform: modelObject asString , ' deactivated.' ] ];
		layoutBuilder: (DynamicTopToBottomColumnsLayout columns: 2);
		build.
	
	^ dynamicPresenter
		openWithSpec;
		yourself
]

{ #category : #example }
DynamicPresentersListBuilder class >> example2 [
	| dynamicPresenter |
	dynamicPresenter := DynamicPresentersListBuilder new
		modelObjects: {1 . 2 . 3 . 4 . 5};
		presenterProvidedBy: [ :integer |
			integer odd "In your real code, avoid conditionals, use dispatch."
				ifTrue: [ CheckBoxPresenter ]
				ifFalse: [ LabelPresenter ] ]
			configuredAs: [ :presenter :modelObject | 
			presenter
				label: modelObject asString ];
		layoutBuilder: (DynamicTopToBottomColumnsLayout columns: 2);
		build.
	
	^ dynamicPresenter
		openWithSpec;
		yourself
]

{ #category : #building }
DynamicPresentersListBuilder >> build [
	^ self buildDynamicPresenter
		layout: self buildLayout;
		yourself
]

{ #category : #building }
DynamicPresentersListBuilder >> buildDynamicPresenter [
	dynamicPresenter := DynamicComposablePresenter new.
	dynamicPresenter instantiatePresenters: self instVarNamesAndPresenterNamesArray.
	
	"Apply configuration block."
	self instVarToModelObjectDo: [ :instVarName :object |
		self configurationBlock value: (instVarName value: dynamicPresenter) value: object ].
	^ dynamicPresenter
]

{ #category : #building }
DynamicPresentersListBuilder >> buildLayout [
	^ self layoutBuilder buildLayoutFor: self
]

{ #category : #accessing }
DynamicPresentersListBuilder >> configurationBlock [
	^ configurationBlock
]

{ #category : #accessing }
DynamicPresentersListBuilder >> configurationBlock: anObject [
	configurationBlock := anObject
]

{ #category : #private }
DynamicPresentersListBuilder >> createNullPresenters: anInteger [
	nullPresentersInstVarNames := self generateNullPresentersInstVarNames: anInteger.

	dynamicPresenter
		instantiatePresenters: self nullPresentersInstVarNamesAndPresenterNamesArray
]

{ #category : #private }
DynamicPresentersListBuilder >> generateNullPresentersInstVarNames: anInteger [
	^ (1 to: anInteger)
		collect: [ :i | self generateUniqueInstanceVariableName ]
]

{ #category : #private }
DynamicPresentersListBuilder >> generateUniqueInstanceVariableName [
	instVarCount := instVarCount + 1.
	^ (#var , instVarCount asString) asSymbol
]

{ #category : #initialization }
DynamicPresentersListBuilder >> initialize [
	super initialize.
	instVarCount := 0.
	nullPresentersInstVarNames := #()
]

{ #category : #private }
DynamicPresentersListBuilder >> instVarNames [
	^ self instVarToModelObject collect: #key
]

{ #category : #private }
DynamicPresentersListBuilder >> instVarNamesAndPresenterNamesArray [
	^ (self instVarToModelObject
		inject: OrderedCollection new
		into: [ :col :instVarToModelObjectAssoc |
			col
				add: instVarToModelObjectAssoc key;
				add: (self presenterBlock value: instVarToModelObjectAssoc value) name;
				yourself ]) asArray
]

{ #category : #private }
DynamicPresentersListBuilder >> instVarToModelObject [
	^ instVarToModelObject
]

{ #category : #private }
DynamicPresentersListBuilder >> instVarToModelObjectDo: aBlock [
	"Iterates on inst. var. names and model object and provides them as argument to the aBlock."
	self instVarToModelObject do: [ :assoc |
		aBlock value: assoc key value: assoc value ]
]

{ #category : #accessing }
DynamicPresentersListBuilder >> layoutBuilder [
	^ layoutBuilder
]

{ #category : #accessing }
DynamicPresentersListBuilder >> layoutBuilder: anObject [
	layoutBuilder := anObject
]

{ #category : #accessing }
DynamicPresentersListBuilder >> modelObjects [
	^ instVarToModelObject collect: #value
]

{ #category : #accessing }
DynamicPresentersListBuilder >> modelObjects: aCollectionOfObjects [
	instVarToModelObject := aCollectionOfObjects collect: [ :o |
		self generateUniqueInstanceVariableName -> o ]
]

{ #category : #private }
DynamicPresentersListBuilder >> nullPresentersInstVarNames [
	^ nullPresentersInstVarNames
]

{ #category : #private }
DynamicPresentersListBuilder >> nullPresentersInstVarNamesAndPresenterNamesArray [
	^ (self nullPresentersInstVarNames
		inject: OrderedCollection new
		into: [ :col :instVar |
			col
				add: instVar;
				add: NullPresenter name;
				yourself ]) asArray
]

{ #category : #api }
DynamicPresentersListBuilder >> presenter: aPresenterClass configuredAs: twoArgumentsBlock [
	self presenterProvidedBy: [ :modelObject | aPresenterClass ] configuredAs: twoArgumentsBlock
]

{ #category : #accessing }
DynamicPresentersListBuilder >> presenterBlock [
	^ presenterBlock
]

{ #category : #accessing }
DynamicPresentersListBuilder >> presenterBlock: anObject [
	presenterBlock := anObject
]

{ #category : #api }
DynamicPresentersListBuilder >> presenterProvidedBy: blockReturningPresenter configuredAs: blockConfiguringThePresenter [
	self
		presenterBlock: blockReturningPresenter;
		configurationBlock: blockConfiguringThePresenter
	
]

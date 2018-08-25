"
Test class for DynamicPresentersListBuilder.
"
Class {
	#name : #DynamicPresentersListBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'Spec-Tests-Dynamic-Builder'
}

{ #category : #running }
DynamicPresentersListBuilderTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
	builder := DynamicPresentersListBuilder new
]

{ #category : #tests }
DynamicPresentersListBuilderTest >> testGenerateNullPresentersInstVarNames [
	builder
		modelObjects: { 1 . 2 . 3 }.
	
	self
		assertCollection: (builder generateNullPresentersInstVarNames: 3) equals: #(var4 var5 var6)
]

{ #category : #tests }
DynamicPresentersListBuilderTest >> testGenerateUniqueInstanceVariableName [
	1 to: 10 do: [ :i |
		self assert: builder generateUniqueInstanceVariableName equals: (#var , i asString) asSymbol ]
]

{ #category : #tests }
DynamicPresentersListBuilderTest >> testInstVarNames [
	builder
		modelObjects: { 3 . 2 . 1 }.
		
	self
		assertCollection: builder instVarNames hasSameElements: #(var1 var2 var3)
]

{ #category : #tests }
DynamicPresentersListBuilderTest >> testInstVarNamesAndPresenterNamesArray [
	builder
		modelObjects: { 3 . 2 . 1 };
		presenterClass: ButtonPresenter.
		
	self
		assertCollection: builder instVarNamesAndPresenterNamesArray hasSameElements: #(var1 ButtonPresenter var2 ButtonPresenter var3 ButtonPresenter)
]

{ #category : #tests }
DynamicPresentersListBuilderTest >> testModelObjects [
	builder
		modelObjects: { 3 . 2 . 1 }.
		
	self
		assertCollection: builder instVarToModelObject hasSameElements: { #var1 -> 3. #var2 -> 2. #var3 -> 1 }
]

{ #category : #tests }
DynamicPresentersListBuilderTest >> testNullPresentersInstVarNamesAndPresenterNamesArray [
	builder
		modelObjects: { 1 . 2 . 3 };
		presenter: CheckBoxPresenter configuredAs: [ :presenter :modelObject | ];
		buildDynamicPresenter;
		createNullPresenters: 3.
	
	self
		assertCollection: builder nullPresentersInstVarNamesAndPresenterNamesArray equals: #(var4 NullPresenter var5 NullPresenter var6 NullPresenter)
]

{ #category : #tests }
DynamicPresentersListBuilderTest >> testPresenterConfiguredAs [
	| block |
	self
		assert: builder presenterClass isNil;
		assert: builder configurationBlock isNil.
	
	block := [ :p :m | ].
	builder
		presenter: ButtonPresenter configuredAs: block.
		
	self
		assert: builder presenterClass equals: ButtonPresenter;
		assert: builder configurationBlock equals: block
]

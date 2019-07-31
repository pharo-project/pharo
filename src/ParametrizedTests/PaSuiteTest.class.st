"
Test case to check that the cases are correctly generated
"
Class {
	#name : #PaSuiteTest,
	#superclass : #ParametrizedTestCase,
	#category : #'ParametrizedTests-Tests'
}

{ #category : #tests }
PaSuiteTest >> testCasesHasTheCorrectNumberOfTests [

	| suite |
	suite := PaCasesMatrixTest buildSuite.
	self assert: suite tests size equals: 3
]

{ #category : #tests }
PaSuiteTest >> testCasesHasTheCorrectTests [

	| suite |
	suite := PaCasesMatrixTest buildSuite.

	"I run the setup to set the parametrized values in the test case"
	suite tests do: #setUp.

	self 
		assertCollection: (suite tests collect: [ :each | {each option1. each option2} ])
		hasSameElements: { { 'a'. 1 }. { 'b'. 2 }. { 'c'. 3 } }

]

{ #category : #tests }
PaSuiteTest >> testCombinedHasTheCorrectNumberOfTests [

	| suite |
	suite := PaCombinedMatrixExampleTest buildSuite.
	self assert: suite tests size equals: 6
]

{ #category : #tests }
PaSuiteTest >> testCombinedHasTheCorrectTests [

	| suite |
	suite := PaCombinedMatrixExampleTest buildSuite.
	
	suite tests do: #setUp.

	self 
		assertCollection: (suite tests collect: [:each | {each option1. each option2. each option3}]) 
		hasSameElements: {	 { 'a'. 1. 'a'}. { 'a'. 1. 'b'}. 
								 { 'b'. 2. 'a'}. { 'b'. 2. 'b'}. 
	 							 { 'c'. 3. 'a'}. { 'c'. 3. 'b'}. }.
]

{ #category : #tests }
PaSuiteTest >> testMatrix3ExampleHasTheCorrectNumberOfTests [

	| suite |
	suite := PaMatrix3ExampleTest buildSuite.
	self assert: suite tests size equals: 27
]

{ #category : #tests }
PaSuiteTest >> testMatrix3ExampleHasTheCorrectTests [

	| suite |
	suite := PaMatrix3ExampleTest buildSuite.
	suite tests do: #setUp.

	self 
		assertCollection: (suite tests collect: [:each | { each option1. each option2. each option3.} ] ) 
		hasSameElements: { 
			{ 'a'. 1. 'x'}. { 'a'. 2. 'x'}. { 'a'. 3. 'x'}.
			{ 'a'. 1. 'y'}. { 'a'. 2. 'y'}. { 'a'. 3. 'y'}.
			{ 'a'. 1. 'z'}. { 'a'. 2. 'z'}. { 'a'. 3. 'z'}.

			{ 'b'. 1. 'x'}. { 'b'. 2. 'x'}. { 'b'. 3. 'x'}.
			{ 'b'. 1. 'y'}. { 'b'. 2. 'y'}. { 'b'. 3. 'y'}.
			{ 'b'. 1. 'z'}. { 'b'. 2. 'z'}. { 'b'. 3. 'z'}.

			{ 'c'. 1. 'x'}. { 'c'. 2. 'x'}. { 'c'. 3. 'x'}.
			{ 'c'. 1. 'y'}. { 'c'. 2. 'y'}. { 'c'. 3. 'y'}.
			{ 'c'. 1. 'z'}. { 'c'. 2. 'z'}. { 'c'. 3. 'z'}.
		}
	
]

{ #category : #tests }
PaSuiteTest >> testMatrixExampleHasTheCorrectNumberOfTests [

	| suite |
	suite := PaMatrixExampleTest buildSuite.
	self assert: suite tests size equals: 9
]

{ #category : #tests }
PaSuiteTest >> testMatrixExampleHasTheCorrectTests [

	| suite |
	suite := PaMatrixExampleTest buildSuite.
	suite tests do: #setUp.


	self 
		assertCollection: (suite tests collect: [:each | { each option1. each option2. } ] ) 
		hasSameElements: { 
			{ 'a'. 1}. { 'a'. 2}. { 'a'. 3}.
			{ 'b'. 1}. { 'b'. 2}. { 'b'. 3}.
			{ 'c'. 1}. { 'c'. 2}. { 'c'. 3}.}
	
]

{ #category : #tests }
PaSuiteTest >> testMultiplyMatrixWithCasesMultiplyCases [

	| m1 m2 m3 |
	m1 := ParametrizedTestMatrix new.
	m1 addCase: { #option1 -> #a . #option2 -> #a }.
	m1 addCase: { #option1 -> #b . #option2 -> #c }.
	
	m2 := ParametrizedTestMatrix new.
	m2 addCase: { #option3 -> #a . #option4 -> #a }.
	m2 addCase: { #option3 -> #b . #option4 -> #c }.
	
	m3 := m1 * m2.
	
	self assert: m3 equals: {
		{ #option1 -> #a. #option2 -> #a. #option3 -> #a. #option4 -> #a  }.
		{ #option1 -> #a. #option2 -> #a. #option3 -> #b. #option4 -> #c  }.
		{ #option1 -> #b. #option2 -> #c. #option3 -> #a. #option4 -> #a  }.
		{ #option1 -> #b. #option2 -> #c. #option3 -> #b. #option4 -> #c  }.
	} asTestMatrix.
]

{ #category : #tests }
PaSuiteTest >> testMultiplyMatrixWithOptionsAndCasesMultiplyCases [

	| m1 m2 m3 |
	m1 := ParametrizedTestMatrix new.
	m1 forSelector: #option1 addOptions: #(a b).
	
	m2 := ParametrizedTestMatrix new.
	m2 addCase: { #option3 -> #a . #option4 -> #a }.
	m2 addCase: { #option3 -> #b . #option4 -> #c }.
	
	m3 := m1 * m2.
	
	self assert: m3 equals: {
		{ #option1 -> #a. #option3 -> #a. #option4 -> #a  }.
		{ #option1 -> #a. #option3 -> #b. #option4 -> #c  }.
		{ #option1 -> #b. #option3 -> #a. #option4 -> #a  }.
		{ #option1 -> #b. #option3 -> #b. #option4 -> #c  }.
	} asTestMatrix.
]

{ #category : #tests }
PaSuiteTest >> testMultiplyMatrixWithOptionsMultiplyCases [

	| m1 m2 m3 |
	m1 := ParametrizedTestMatrix new.
	m1 forSelector: #option1 addOptions: #(a b).
	
	m2 := ParametrizedTestMatrix new.
	m2 forSelector: #option2 addOptions: #(a b).
	
	m3 := m1 * m2.
	
	self assert: m3 equals: {
		{ #option1 -> #a. #option2 -> #a }.
		{ #option1 -> #a. #option2 -> #b }.
		{ #option1 -> #b. #option2 -> #a }.
		{ #option1 -> #b. #option2 -> #b }.
	} asTestMatrix.
]

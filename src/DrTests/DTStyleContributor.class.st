Class {
	#name : 'DTStyleContributor',
	#superclass : 'StPharoStyleContributor',
	#category : 'DrTests-Spec',
	#package : 'DrTests',
	#tag : 'Spec'
}

{ #category : 'styles' }
DTStyleContributor >> styleSheetContribution [

	^ SpStyle newApplication
		addClass: 'testError' with: [ :class |
			class addPropertyDrawWith: [ :draw |
				draw backgroundColor: TestResult defaultColorBackGroundForErrorTest ] ];
		"addClass: 'testExpectedFailure' with: [ :class |
			class addPropertyDrawWith: [ :draw |
				draw backgroundColor: Color transparent ] ];"
		addClass: 'testFail' with: [ :class |
			class addPropertyDrawWith: [ :draw |
				draw backgroundColor: TestResult defaultColorBackGroundForFailureTest ] ];
		addClass: 'testPass' with: [ :class |
			class addPropertyDrawWith: [ :draw |
				draw backgroundColor:   TestResult defaultColorBackGroundForPassingTest ] ];
		addClass: 'testSkipped' with: [ :class |
			class addPropertyDrawWith: [ :draw |
				draw backgroundColor:  TestResult defaultColorBackGroundForPassingTest ] ];
		"addClass: 'testUnexpectedPass' with: [ :class |
			class addPropertyDrawWith: [ :draw |
				draw backgroundColor: Color gray ] ];"
		yourself
]

{ #category : 'styles' }
DTStyleContributor >> theme [

	^ Smalltalk ui theme
]

Class {
	#name : #DTStyleContributor,
	#superclass : #StPharoStyleContributor,
	#category : #'DrTests-Spec'
}

{ #category : #styles }
DTStyleContributor >> styleSheetContribution [

	^ SpStyle newApplication 
		addClass: 'testError' with: [ :class | 
			class addPropertyDrawWith: [ :draw | 
				draw backgroundColor: self theme testColorError ] ];
		"addClass: 'testExpectedFailure' with: [ :class | 
			class addPropertyDrawWith: [ :draw | 
				draw backgroundColor: Color transparent ] ];"
		addClass: 'testFail' with: [ :class | 
			class addPropertyDrawWith: [ :draw | 
				draw backgroundColor: self theme testColorFailure ] ];
		addClass: 'testPass' with: [ :class | 
			class addPropertyDrawWith: [ :draw | 
				draw backgroundColor:  self theme testColorPassing ] ];
		addClass: 'testSkipped' with: [ :class | 
			class addPropertyDrawWith: [ :draw | 
				draw backgroundColor: self theme testColorPassing ] ];
		"addClass: 'testUnexpectedPass' with: [ :class | 
			class addPropertyDrawWith: [ :draw | 
				draw backgroundColor: Color gray ] ];"
		yourself
]

{ #category : #styles }
DTStyleContributor >> theme [
		
	^ Smalltalk ui theme
]

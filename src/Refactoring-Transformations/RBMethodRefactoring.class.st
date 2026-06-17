"
I am an abstract base class for method refactorings.

I only provide a helper method for generating  selector names.
"
Class {
	#name : 'RBMethodRefactoring',
	#superclass : 'ReRefactoring',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'testing' }
RBMethodRefactoring class >> isAbstract [

	^ self == RBMethodRefactoring
]

{ #category : 'accessing' }
RBMethodRefactoring >> methodClass [
	^ class
]

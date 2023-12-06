"
I am a condition that checks if a subtree contains at most one assignment that has references outside of subtree.

When we have one assignment to temp or arg we don't need to worry about other references,
since we can return newly assigned value from the extracted method and preserve behavior.
When we have two or more assignments AND when they have references outside of extracted block,
we don't support return of multiple values, instead we notify the user that all references
to those temps should be extracted as well.
"
Class {
	#name : 'RBMaxOneAssignmentWithReferencesCondition',
	#superclass : 'RBSubtreeCondition',
	#instVars : [
		'assignments'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
RBMaxOneAssignmentWithReferencesCondition >> assignments: aCollection [ 

	assignments := aCollection
]

{ #category : 'checking' }
RBMaxOneAssignmentWithReferencesCondition >> check [ 

	assignments ifNil: [ ^ false ].
	
	^ assignments size <= 1
]

{ #category : 'displaying' }
RBMaxOneAssignmentWithReferencesCondition >> violationMessageOn: aStream [

	aStream
		nextPutAll:
			'Cannot extract two or more assignments to temporaries without also extracting all the references.';
		cr;
		nextPutAll: assignments asString;
		cr;
		nextPutAll:
			'Either reduce selection to remove one assignment or extend it to include all references to all temporaries.'
]

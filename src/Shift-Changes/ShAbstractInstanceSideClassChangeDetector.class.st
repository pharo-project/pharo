"
I am an special Class change detector, used when we have to include the change in the instance structure.
"
Class {
	#name : #ShAbstractInstanceSideClassChangeDetector,
	#superclass : #ShAbstractClassChangeDetector,
	#category : 'Shift-Changes'
}

{ #category : #changes }
ShAbstractInstanceSideClassChangeDetector >> newChanges [
	^ {ShClassChanged new
		builder: builder;
		yourself.
		
		ShInstanceShapeChanged new
		builder: builder;
		yourself.
		} .
]

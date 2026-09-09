Extension { #name : 'Object' }

{ #category : '*Instrumentation' }
Object >> insInequality: anObject [
    "Answer whether the receiver and the argument do not represent the
	same object."

    <noInstrumentation>
    ^ (self insIsEqual: anObject) insIsEqual: false
]

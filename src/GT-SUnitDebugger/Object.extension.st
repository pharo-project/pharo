Extension { #name : #Object }

{ #category : #'*GT-SUnitDebugger' }
Object >> gtDebuggerSUnitPrint [
	"I return a textual representation of the object that is used by the SUnit debugger to compare objects using a textual diff."
	
	^ self printString
]

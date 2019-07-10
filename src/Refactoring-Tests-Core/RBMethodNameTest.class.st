Class {
	#name : #RBMethodNameTest,
	#superclass : #TestCase,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBMethodNameTest >> testCreation [ 
	"Yes this is stupid test but it would have save time to understand what should be passed as argument!"
	
	| rbmethod |
	rbmethod := RBMethodName selector: #foo: arguments: #('x').
	self assert: rbmethod selector equals: #foo:.
	self assert: rbmethod arguments equals: #('x').
	
]

{ #category : #tests }
RBMethodNameTest >> testIsValid [
	"A valid RBMethodName is a method name with a valid selector and corresponding arguments"
	
	| rbmethod |
	rbmethod := RBMethodName selector: #foo: arguments: #('x').
	self assert: rbmethod isValid.
	
	rbmethod := RBMethodName selector: #foo:arg: arguments: #('x' 'y').
	self assert: rbmethod isValid.
	
	rbmethod := RBMethodName selector: #foo:arg: arguments: #('x').
	self deny: rbmethod isValid.
	
	rbmethod := RBMethodName selector: #foo: arguments: #('x' 'y').
	self deny: rbmethod isValid.
	
	
	rbmethod := RBMethodName selector: 'fggfg gg g ' arguments: #('x').
	self deny: rbmethod isValid.
]

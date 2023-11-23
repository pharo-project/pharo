Class {
	#name : 'ClyMethodSourceCodeContext',
	#superclass : 'ClySourceCodeContext',
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'testing' }
ClyMethodSourceCodeContext >> isMethodSelected [

	^self selectedSourceNode isMethod
]

{ #category : 'tool controlling' }
ClyMethodSourceCodeContext >> lastSelectedClass [ 
	^ self selectedMethod methodClass
]

{ #category : 'selection' }
ClyMethodSourceCodeContext >> lastSelectedMethod [
	^self selectedMethod
]

{ #category : 'selection' }
ClyMethodSourceCodeContext >> selectedMethod [
	^tool editingMethod
]

{ #category : 'selection' }
ClyMethodSourceCodeContext >> selectedMethods [
	^{self selectedMethod}
]

{ #category : 'tool controlling' }
ClyMethodSourceCodeContext >> showMessage: aMessageDescription renamedTo: newSelector [
	"here we expect only messages which was created from ast-nodes"
	tool editingMethod selector == aMessageDescription selector
		ifTrue: [ tool switchToMethod: tool methodClass >> newSelector]
		ifFalse: [ tool selectAnySelector: { newSelector } ]
]

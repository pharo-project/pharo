Class {
	#name : #CompiledCodeTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #accessing }
CompiledCodeTest >> compiledMethod1 [
	^ self class >> #method1
]

{ #category : #accessing }
CompiledCodeTest >> method1 [
	
	<pragma1: 123 foo: 'bar' >
	| array | 
	array := Array new.
	array at: 1 put: 'Pharo loves tests'.
	#(#add #at: #remove) do: #printOn:
]

{ #category : #accessing }
CompiledCodeTest >> testLiteralsDoesNotContainMethodClass [
	
	self
		deny:
			(self compiledMethod1
				hasLiteralThorough: (self class environment associationAt: self class name asSymbol))
]

{ #category : #accessing }
CompiledCodeTest >> testLiteralsDoesNotContainMethodName [

	self deny: (self compiledMethod1 hasLiteralThorough: #method1)
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsFalseWhenLiteralNotIsInMethodPropertiesKey [
	
	[ self compiledMethod1 propertyAt: #Once put: true.
	self deny: (self compiledMethod1 hasLiteralThorough: #Absent) ]
		ensure: [ self compiledMethod1 removeProperty: #Once ]
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsArrayOfLiterals [

	self assert: (self compiledMethod1 hasLiteralThorough: #(#add #at: #remove))
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsByteString [

	self assert: (self compiledMethod1 hasLiteralThorough: 'Pharo loves tests')
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsByteSymbol [

	self assert: (self compiledMethod1 hasLiteralThorough: #printOn:)
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsContainedInArrayOfLitterals [

	self assert: (self compiledMethod1 hasLiteralThorough: #add)
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsGlobalVariable [
	
	self assert: (self compiledMethod1
				hasLiteralThorough: (self class environment associationAt: #Array))
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInMethodPropertiesKey [
	
	[ self compiledMethod1 propertyAt: #Once put: true.
	self assert: (self compiledMethod1 hasLiteralThorough: #Once) ]
		ensure: [ self compiledMethod1 removeProperty: #Once ]
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInMethodPropertiesValue [
	
	[ self compiledMethod1 propertyAt: #Once put: '123'.
	self assert: (self compiledMethod1 hasLiteralThorough: '123') ]
		ensure: [ self compiledMethod1 removeProperty: #Once ]
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInMethodPropertiesValueArray [
	
	[ self compiledMethod1 propertyAt: #Once put: #(1 2 3).
	self assert: (self compiledMethod1 hasLiteralThorough: 1) ]
		ensure: [ self compiledMethod1 removeProperty: #Once ]
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInPragmaArguments [
	
	self assert: (self compiledMethod1 hasLiteralThorough: 'bar').
	self assert: (self compiledMethod1 hasLiteralThorough: 123)
]

{ #category : #accessing }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInPragmaSelector [
	
	self assert: (self compiledMethod1
				hasLiteralThorough: #pragma1:foo:)
]

Class {
	#name : 'OCCompilationContextTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Plugins',
	#package : 'OpalCompiler-Tests',
	#tag : 'Plugins'
}

{ #category : 'tests' }
OCCompilationContextTest >> testMultipleParseOptionsStoreTheResult [

	| cc |
	cc := OCCompilationContext new. 
	cc parseOptions: #( a ).
	cc parseOptions: #( b ).
	cc parseOptions: #( c). 
	self assert: cc options equals: #(a b c) asSet.  
]

{ #category : 'tests' }
OCCompilationContextTest >> testParseOptionsEmptyResultsInEmptyResult [

	| cc |
	cc := OCCompilationContext new. 
	cc parseOptions: #( ).
	self assert: cc options equals: #() asSet.  
]

{ #category : 'tests' }
OCCompilationContextTest >> testParseOptionsMultipleAdds [

	| cc |
	cc := OCCompilationContext new. 
	cc parseOptions: #( a b c a b c c c c).
	
	self assert: cc options equals: #(a b c) asSet.  
]

{ #category : 'tests' }
OCCompilationContextTest >> testParseOptionsMultipleAddsAndRemoveOnceResultsInEmptySet [

	| cc |
	cc := OCCompilationContext new. 
	cc parseOptions: #( a b c a b c c c c #- a b c ).
	
	self assert: cc options equals: #( ) asSet.  
]

{ #category : 'tests' }
OCCompilationContextTest >> testParseOptionsRemoveNotAddedResultsInEmptySet [

	| cc |
	cc := OCCompilationContext new. 
	cc parseOptions: #(#- a b c ).
	
	self assert: cc options equals: #( ) asSet.  
]

{ #category : 'tests' }
OCCompilationContextTest >> testParseOptionsWithoutDirectiveIsAddition [

	| cc |
	cc := OCCompilationContext new. 
	cc parseOptions: #( a b c).
	
	self assert: cc options equals: #(a b c) asSet.  
]

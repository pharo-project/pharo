Class {
	#name : 'ClapMatchGraphBuilderTest',
	#superclass : 'TestCase',
	#category : 'Clap-UI-Tests',
	#package : 'Clap-UI-Tests'
}

{ #category : 'tests' }
ClapMatchGraphBuilderTest >> testCanGetNodesFromMatch [

	| context builder nodes |
	context := ClapCommandLineExamples hello activationWith: #( 'hello' 'Alice' '--shout' '--language' 'fr' ).
	builder := ClapMatchGraphBuilder forContext: context.
	
	nodes := builder graphNodes.
	
	self assert: nodes size equals: 5.
	self 
		assertCollection: (nodes collect: #printString) 
		hasSameElements: #('a ClapNamedMatch(hello)' 'a ClapWordMatch(Alice)' 'a ClapNamedMatch(--shout)' 'a ClapNamedMatch(--language)' 'a ClapWordMatch(fr)')
]

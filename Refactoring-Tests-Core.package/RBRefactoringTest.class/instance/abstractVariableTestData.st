private
abstractVariableTestData
	| newModel classEnvironment classes |
	classes := #(#Bar #Foo) 
		inject: OrderedCollection new 
		into: [ :sum :each | 
			Smalltalk globals at: each ifPresent: [ :class | 
				sum add: class; add: class class ].
			sum ].
	classEnvironment := RBClassEnvironment classes: classes.
	newModel := RBNamespace onEnvironment: classEnvironment not.
	newModel name: 'Test'.
	
	#('Object subclass: #Foo
		instanceVariableNames: ''instVarName1 instVarName2''
		classVariableNames: ''ClassVarName1 ClassVarName2 ''
		poolDictionaries: ''''
		category: ''Testing'' '
	'Foo subclass: #Bar
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' '
	) do: [ :each | 
		newModel defineClass: each ].
	
	#(#(#Bar #(
		#('instVarName1
			^1242321' #tests) 
		#('foo
			instVarName1 := instVarName1 + instVarName2 + ClassVarName1' #tests))) 
	#(#Foo #(
		#('foo
			^instVarName2 := 3' #tests)
		#('bar
			"Add one to instVarName1"
			instVarName1 := instVarName1 + 1' #tests) 
		#('classVarName1
			^ClassVarName1' #tests)
		#('instVarName1: anObject
			^anObject' #tests)
		#('asdf
			^self classVarName1: (ClassVarName1 := ClassVarName1 + 1)' #tests) 
		#('instVarName2
			^instVarName2' #tests)
		#('instVarName2: anObject
			instVarName2 := anObject' #tests)
		#('classVarName1: anObject
			^ClassVarName1 := anObject' #tests)))
	
	#('Bar class' #(
		#('classVarName2: anObject
			ClassVarName2 := anObject' #tests) 
		#('classVarName2
			^ClassVarName2' #tests))) 
	#('Foo class' #(
		#('foo
			^ClassVarName1 := ClassVarName1 * ClassVarName1 * ClassVarName1' #tests)))) 
		do: [:each | 
			| class |
			class := newModel classNamed: each first.
			each last do: [ :methodPair | 
				class 
					compile: methodPair first
					classified: methodPair last]].
	^ newModel
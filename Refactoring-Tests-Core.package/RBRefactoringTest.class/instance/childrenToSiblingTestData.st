private
childrenToSiblingTestData
^'
| m |
(m:= RBNamespace 
			onEnvironment: ((RBClassEnvironment onEnvironment: RBBrowserEnvironment new) 
					classes: (#(#ConcreteSubclass #ConcreteSuperclass #NoMoveSubclass) 
							inject: OrderedCollection new
							into: 
								[:sum :each | 
								| class |
								class := Smalltalk at: each ifAbsent: [nil].
								class notNil ifTrue: [sum add: class].
								sum]) 
								, (#(#ConcreteSubclass #ConcreteSuperclass #NoMoveSubclass) 
										inject: OrderedCollection new
										into: 
											[:sum :each | 
											| class |
											class := Smalltalk at: each ifAbsent: [nil].
											class notNil ifTrue: [sum add: class class].
											sum])) 
						not) 
		name: ''Test''.
#(''ConcreteSuperclass subclass: #NoMoveSubclass
	instanceVariableNames: ''''''''
	classVariableNames: ''''''''
	poolDictionaries: ''''''''
	category: ''''Testing'''''' ''Object subclass: #ConcreteSuperclass
	instanceVariableNames: ''''instVarName1 instVarName2 ''''
	classVariableNames: ''''ClassVarName1 ClassVarName2 ''''
	poolDictionaries: ''''''''
	category: ''''Testing'''''' ''ConcreteSuperclass subclass: #ConcreteSubclass
	instanceVariableNames: ''''''''
	classVariableNames: ''''''''
	poolDictionaries: ''''''''
	category: ''''Testing'''''') 
	do: [:each | m defineClass: each].
(m metaclassNamed: #ConcreteSuperclass) 
	addInstanceVariable: ''classInstVarName1''.
#(#(#ConcreteSubclass #(#(''initialize
	super initialize.
	instVarName1 := nil'' #everyone) #(''different
	^0'' #everyone))) #(#ConcreteSuperclass #(#(''same
	^self initialize isKindOf: ConcreteSuperclass'' #''one def'') #(''different
	^instVarName1 + instVarName2'' #everyone) #(''initialize
	instVarName1 := instVarName2 := ClassVarName1 := ClassVarName2 := 0'' #everyone))) #(#NoMoveSubclass #(#(''same
	^123'' #''one def''))) #(''ConcreteSubclass class'' #(#(''bar
	^self storeString'' #testing))) #(''ConcreteSuperclass class'' #(#(''foo
	^classInstVarName1 + ClassVarName1 + ClassVarName2'' #testing) #(''new
	^super new initialize'' #testing) #(''bar
	^self printString'' #testing))) #(''NoMoveSubclass class'' #())) 
	do: 
		[:each | 
		| class |
		class := m classNamed: each first.
		each last do: [:meth | class compile: meth first classified: meth last]].
m
'
"
Test for functional style programming using #chain message (which involves `ChainProxy`)
"
Class {
	#name : #ChainProxyTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Utilities'
}

{ #category : #tests }
ChainProxyTest >> testChainingInitiallyReturnsProxy [

	self assert: #foo chain class equals: ChainProxy
]

{ #category : #tests }
ChainProxyTest >> testFurtherChainingReturnsResultOfLastMessage [

	| proxy result1 result2 result3 |
	proxy := #(apple peach banana) chain.
	
	"First result of a message #groupedBy: sent to the proxy"
	result1 := proxy groupedBy: #size.		
	self assert: result1 size equals: 2.
	self assert: result1 class equals: OrderedDictionary. 

	result2 := result1 select: [:each | each size even ].
	self assert: result2 size equals: 1.
	
	result3 := result2 values.
	self assert: result3 size equals: 1.
	self assert: result3 first equals: #(apple peach) 
]

{ #category : #tests }
ChainProxyTest >> testMoreCleanCodeWithChaining [

	| result |
	result := #( apple peach banana ) chain
		          groupedBy: #size;
		          select: [ :each | each size even ];
		          values;
		          collect: #asCommaString.				 
		
	self assert: result equals: #('apple, peach')
]

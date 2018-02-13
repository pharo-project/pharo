"
To do:
	- radix should be returned 
"
Class {
	#name : #MethodFinderTest,
	#superclass : #TestCase,
	#category : #'Tools-Test-Finder'
}

{ #category : #'test examples' }
MethodFinderTest >> testAbs [
		
	self assert: (MethodFinder new load: #((4) 4  (-10) 10 (-3) 3 (2) 2 (-6) 6 (612) 612)) searchForOne asArray equals: #('data1 abs')
]

{ #category : #'test examples' }
MethodFinderTest >> testAllNumbers [
		
	self assert: (MethodFinder new load: #((5) 0.2   (2) 0.5)) allNumbers.
	self deny: (MethodFinder new load: #(('' $A) 'A')) allNumbers
]

{ #category : #'test examples' }
MethodFinderTest >> testDegreeSin [
		
	self assert: (MethodFinder new load: #( (0) 0  (30) 0.5  (45) 0.707106  (90) 1)) searchForOne asArray = #('data1 degreeSin') 
]

{ #category : #'test examples' }
MethodFinderTest >> testDivision [

	self 
		assert: (MethodFinder new load: #((7 5) 2   (4 5) 4   (-9 4) 3)) searchForOne asArray 
		equals: #('data1 \\ data2').
	self assert:
		(MethodFinder new load: #((35) 3   (17) 1   (5) 5)) searchForOne asArray 
		equals: #('data1 \\ 8').
	self assert: (MethodFinder new load: #((12 4 8) 2  (1 3 6) 2  (5 2 16) 8)
	) searchForOne asArray equals:  #().
]

{ #category : #'test examples' }
MethodFinderTest >> testGreaterThanANumber [

	self 
		assert: (MethodFinder new load: {#(7). true.   #(4.1).  true.   #(1.5). false}) searchForOne asArray 
		equals: #('data1 >= 4.1').
		
	self assert: (MethodFinder new load: {#(4 3). true.  #(-7 3). false.  #(5 1). true.  #(5 5). false}) searchForOne asArray 
		equals: #('data1 > data2').
	self assert: (MethodFinder new load: #((36) 7   (50) 10)) searchForOne asSortedCollection asArray equals: #( 'data1 // 5' 'data1 quo: 5')
]

{ #category : #'test examples' }
MethodFinderTest >> testIfTrueIfFalse [

	self assert: (MethodFinder new load:  { { true. [3]. [4]}. 3.  { false. [0]. [6]}. 6}) searchForOne asArray = #('data1 ifTrue: data2 ifFalse: data3') 
]

{ #category : #'test examples' }
MethodFinderTest >> testIsOdd [
		
	self assert: (MethodFinder new load: {#(1). true. #(2). false. #(5). true. #(10). false}) searchForOne asArray = #('data1 odd')
]

{ #category : #'test examples' }
MethodFinderTest >> testIsRadix [

	self assert: ((MethodFinder new load: #((4 2) '2r100'   (255 16) '16rFF'    (14 8) '8r16')) searchForOne) asArray = #('data1 storeStringBase: data2')
		
	"Apparently the system should find :
	the #('data1 radix: data2' 'data1 printStringBase: data2' 'data1 storeStringBase: data2')"
]

{ #category : #'test examples' }
MethodFinderTest >> testMethodFor [
	
	self assert: (MethodFinder  methodFor: {#(1). true. #(2). false. #(5). true. #(10). false}) =  '(data1 allMask: 1) (data1 anyMask: 1) (data1 odd) '
]

{ #category : #'test examples' }
MethodFinderTest >> testPointY [
	
	self assertCollection:(MethodFinder new load: {{Point x: 3 y: 4}. 4.  {Point x: 1 y: 5}. 5}) searchForOne  hasSameElements: #('data1 max' 'data1 y').
	self assertCollection:(MethodFinder new load: {{Point x: 5 y: 4}. 4.  {Point x: 1 y: 5}. 5}) searchForOne  hasSameElements: #( 'data1 y').
]

{ #category : #'test examples' }
MethodFinderTest >> testPolynomial [
		
	self assert: (MethodFinder new load: #( ((2 3) 2) 8   ((2 3) 5) 17 )) searchForOne asArray 
		equals: #('data1 polynomialEval: data2').	
	self assert: (MethodFinder new load: #((2) 8   (5) 17 )) searchForOne asArray 
		equals: #('#(2 3) polynomialEval: data1')	

]

{ #category : #'test examples' }
MethodFinderTest >> testPower [
		
	self assert:(MethodFinder new load: #((7) 2   (4) 2 )) searchForOne asArray equals: #('^ 2')
]

{ #category : #'test examples' }
MethodFinderTest >> testReciprocal [
		
	self assert: (MethodFinder new load: #((5) 0.2   (2) 0.5)) searchForOne asArray = #('data1 reciprocal')

]

{ #category : #'test examples' }
MethodFinderTest >> testSearchForAccessAtOne [
		
	self
		assert: (MethodFinder new load: #(('abcd' 1) $a  ('Ted ' 3) $d ))
			searchForOne asArray sorted
		equals: #('data1 at: data2' 'data1 atPin: data2' 'data1 atWrap: data2') sorted
		 
]

{ #category : #'test examples' }
MethodFinderTest >> testSearchForOne [
		
	self
		assert: (MethodFinder new load: #(('abcd') $a  ('TedK') $T))
			searchForOne asSortedCollection asArray
		equals: #('data1 anyOne' 'data1 first')
		 
]

{ #category : #'test examples' }
MethodFinderTest >> testSubstraction [
		
	self assert: (MethodFinder new load: #((14 3) 11  (-10 5) -15  (4 -3) 7)) searchForOne asArray equals: #('data1 - data2').
	
	self assert: (MethodFinder new load: #(((12 4 8)) 24  ((1 3 6)) 10 )
	) searchForOne asArray  equals: #('data1 sum') 
]

"
To do:
	- radix should be returned 
"
Class {
	#name : #MethodFinderUITest,
	#superclass : #TestCase,
	#category : #'Tools-Test-Finder'
}

{ #category : #'test examples' }
MethodFinderUITest >> testAbs [
		
	self assert: (MethodFinderUI new load: #((4) 4  (-10) 10 (-3) 3 (2) 2 (-6) 6 (612) 612)) searchForOne asArray equals: #('data1 abs')
]

{ #category : #'test examples' }
MethodFinderUITest >> testAllNumbers [
		
	self assert: (MethodFinderUI new load: #((5) 0.2   (2) 0.5)) allNumbers.
	self deny: (MethodFinderUI new load: #(('' $A) 'A')) allNumbers
]

{ #category : #'test examples' }
MethodFinderUITest >> testDegreeSin [
		
	self assert: (MethodFinderUI new load: #( (0) 0  (30) 0.5  (45) 0.707106  (90) 1)) searchForOne asArray = #('data1 degreeSin') 
]

{ #category : #'test examples' }
MethodFinderUITest >> testDivision [

	self 
		assert: (MethodFinderUI new load: #((7 5) 2   (4 5) 4   (-9 4) 3)) searchForOne asArray 
		equals: #('data1 \\ data2').
	self assert:
		(MethodFinderUI new load: #((35) 3   (17) 1   (5) 5)) searchForOne asArray 
		equals: #('data1 \\ 8').
	self assert: (MethodFinderUI new load: #((12 4 8) 2  (1 3 6) 2  (5 2 16) 8)
	) searchForOne asArray equals:  #().
]

{ #category : #'test examples' }
MethodFinderUITest >> testGreaterThanANumber [

	self 
		assert: (MethodFinderUI new load: {#(7). true.   #(4.1).  true.   #(1.5). false}) searchForOne asArray 
		equals: #('data1 >= 4.1').
		
	self assert: (MethodFinderUI new load: {#(4 3). true.  #(-7 3). false.  #(5 1). true.  #(5 5). false}) searchForOne asArray 
		equals: #('data1 > data2').
	self assert: (MethodFinderUI new load: #((36) 7   (50) 10)) searchForOne asSortedCollection asArray equals: #( 'data1 // 5' 'data1 quo: 5')
]

{ #category : #'test examples' }
MethodFinderUITest >> testIfTrueIfFalse [

	self assert: (MethodFinderUI new load:  { { true. [3]. [4]}. 3.  { false. [0]. [6]}. 6}) searchForOne asArray = #('data1 ifTrue: data2 ifFalse: data3') 
]

{ #category : #'test examples' }
MethodFinderUITest >> testIsOdd [
		
	self assert: (MethodFinderUI new load: {#(1). true. #(2). false. #(5). true. #(10). false}) searchForOne asArray = #('data1 odd')
]

{ #category : #'test examples' }
MethodFinderUITest >> testIsRadix [

	self assert: ((MethodFinderUI new load: #((4 2) '2r100'   (255 16) '16rFF'    (14 8) '8r16')) searchForOne) asArray = #('data1 storeStringBase: data2')
		
	"Apparently the system should find :
	the #('data1 radix: data2' 'data1 printStringBase: data2' 'data1 storeStringBase: data2')"
]

{ #category : #'test examples' }
MethodFinderUITest >> testMethodFor [
	
	self assert: (MethodFinderUI  methodFor: {#(1). true. #(2). false. #(5). true. #(10). false}) =  '(data1 allMask: 1) (data1 anyMask: 1) (data1 odd) '
]

{ #category : #'test examples' }
MethodFinderUITest >> testPointY [
	
	self assertCollection:(MethodFinderUI new load: {{Point x: 3 y: 4}. 4.  {Point x: 1 y: 5}. 5}) searchForOne  hasSameElements: #('data1 max' 'data1 y').
	self assertCollection:(MethodFinderUI new load: {{Point x: 5 y: 4}. 4.  {Point x: 1 y: 5}. 5}) searchForOne  hasSameElements: #( 'data1 y').
]

{ #category : #'test examples' }
MethodFinderUITest >> testPolynomial [
		
	self assert: (MethodFinderUI new load: #( ((2 3) 2) 8   ((2 3) 5) 17 )) searchForOne asArray 
		equals: #('data1 polynomialEval: data2').	
	self assert: (MethodFinderUI new load: #((2) 8   (5) 17 )) searchForOne asArray 
		equals: #('#(2 3) polynomialEval: data1')	

]

{ #category : #'test examples' }
MethodFinderUITest >> testPower [
		
	self assert:(MethodFinderUI new load: #((7) 2   (4) 2 )) searchForOne asArray equals: #('^ 2')
]

{ #category : #'test examples' }
MethodFinderUITest >> testReciprocal [
		
	self assert: (MethodFinderUI new load: #((5) 0.2   (2) 0.5)) searchForOne asArray = #('data1 reciprocal')

]

{ #category : #'test examples' }
MethodFinderUITest >> testSearchForAccessAtOne [
		
	self
		assert: (MethodFinderUI new load: #(('abcd' 1) $a  ('Ted ' 3) $d ))
			searchForOne asArray sorted
		equals: #('data1 at: data2' 'data1 atPin: data2' 'data1 atWrap: data2') sorted
		 
]

{ #category : #'test examples' }
MethodFinderUITest >> testSearchForOne [
		
	self
		assert: (MethodFinderUI new load: #(('abcd') $a  ('TedK') $T))
			searchForOne asSortedCollection asArray
		equals: #('data1 anyOne' 'data1 first')
		 
]

{ #category : #'test examples' }
MethodFinderUITest >> testSubstraction [
		
	self assert: (MethodFinderUI new load: #((14 3) 11  (-10 5) -15  (4 -3) 7)) searchForOne asArray equals: #('data1 - data2').
	
	self assert: (MethodFinderUI new load: #(((12 4 8)) 24  ((1 3 6)) 10 )
	) searchForOne asArray  equals: #('data1 sum') 
]

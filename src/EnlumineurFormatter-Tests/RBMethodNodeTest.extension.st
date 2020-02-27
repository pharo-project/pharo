Extension { #name : #RBMethodNodeTest }

{ #category : #'*EnlumineurFormatter-Tests' }
RBMethodNodeTest >> testBlockHasArguments [

	| tree |
	tree := self parseMethod: 'foo 
		[ :arg | 
		abd + cde 
		]'.

	self assert: tree body statements first hasArguments
	

]

{ #category : #'*EnlumineurFormatter-Tests' }
RBMethodNodeTest >> testBlockHasTemporaries [

	| tree |
	tree := self parseMethod: 'foo 
		[ 
		| tmp |
		abd + cde 
		]'.

	self deny: tree hasTemporaries.
	self assert: tree body statements first hasTemporaries
	

]

{ #category : #'*EnlumineurFormatter-Tests' }
RBMethodNodeTest >> testBlockHeadIsNotEmpty [

	| tree |
	tree := self parseMethod: 'foo 
		[ :arg | 
		abd + cde 
		]'.

	self assert: tree body statements first headIsNotEmpty 
	

]

{ #category : #'*EnlumineurFormatter-Tests' }
RBMethodNodeTest >> testBlockHeadIsNotEmpty2 [

	| tree |
	tree := self parseMethod: 'foo 
		[ "blbl" 
		abd + cde 
		]'.

	self assert: tree body statements first headIsNotEmpty 
	

]

{ #category : #'*EnlumineurFormatter-Tests' }
RBMethodNodeTest >> testBlockHeadIsNotEmpty3 [

	| tree |
	tree := self parseMethod: 'foo 
		[ 
		abd + cde 
		]'.

	self deny: tree body statements first headIsNotEmpty 
	

]

{ #category : #'*EnlumineurFormatter-Tests' }
RBMethodNodeTest >> testMethodsHasTemporaries [

	| tree |
	tree := self parseMethod: 'foo: abd bar: cde  
		| tmp |
^ abd + cde'.

	self assert: tree hasTemporaries
	

]

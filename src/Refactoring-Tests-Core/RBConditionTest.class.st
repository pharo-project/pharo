Class {
	#name : #RBConditionTest,
	#superclass : #TestCase,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBConditionTest >> testCheckFailEarlyAndDoesNotCoverEveryConditions [

	| falseCase trueCase composition |
	falseCase := RBCondition
		             withBlock: [ false ]
		             errorString: 'The false case'.
	trueCase := RBCondition
		            withBlock: [ false ]
		            errorString: 'The second false case'.
	composition := falseCase & trueCase.
	self deny: composition check.
	self
		assert: composition errorString
		equals: 'The false case'
]

{ #category : #tests }
RBConditionTest >> testCheckInvalidMethodName [
	"Usually used to validate input."

	self deny: (RBCondition checkMethodName: 'fofo fo').
	self deny: (RBCondition checkMethodName: '123fofo').
	self deny: (RBCondition checkMethodName: 'foo::').
	self deny: (RBCondition checkMethodName: 'agr:goo:aa').
	self deny: (RBCondition checkMethodName: 'foo:123:').
	self deny: (RBCondition checkMethodName: 'foo[arg]').
	self deny: (RBCondition checkMethodName: 'foo:=arg').
	self deny: (RBCondition checkMethodName: 'foo:arg)').
	self deny: (RBCondition checkMethodName: 'foo:(arg)').
	self deny: (RBCondition checkMethodName: 'foo:+arg)').
	self deny: (RBCondition checkMethodName: '<<foo:<<arg)').
	self deny: (RBCondition checkMethodName: 'foo:agr^:').
	self deny: (RBCondition checkMethodName: 'foo:agr')
]

{ #category : #tests }
RBConditionTest >> testCheckThatAndIsWorking [

	| falseCase trueCase composition |
	falseCase := RBCondition
		             withBlock: [ false ]
		             errorString: 'The false case'.
	trueCase := RBCondition
		            withBlock: [ true ]
		            errorString: 'The true case'.
	composition := falseCase & trueCase.
	self deny: composition check
]

{ #category : #tests }
RBConditionTest >> testCheckThatOnlyFailingConditionErrorIsReported [

	| falseCase trueCase composition |
	falseCase := RBCondition
		             withBlock: [ false ]
		             errorString: 'The false case'.
	trueCase := RBCondition
		            withBlock: [ true ]
		            errorString: 'The true case'.
	composition := falseCase & trueCase.
	self deny: composition check.
	self assert: composition errorString equals: 'The false case'
]

{ #category : #tests }
RBConditionTest >> testCheckThatOnlyFailingConditionErrorIsReportedTrueCaseFirst [

	| falseCase trueCase composition |
	trueCase := RBCondition
		             withBlock: [ true ]
		             errorString: 'The true case'.
	falseCase := RBCondition
		            withBlock: [ false ]
		            errorString: 'The false case'.
	composition := trueCase & falseCase.
	self deny: composition check.
	self
		assert: composition errorString
		equals: 'The false case'
]

{ #category : #tests }
RBConditionTest >> testCheckValidMethodName [
	"Usually used to validate input."
	self assert: (RBCondition checkMethodName: 'foo').
	self assert: (RBCondition checkMethodName: #foo).

	self assert: (RBCondition checkMethodName: #+)
]

{ #category : #tests }
RBConditionTest >> testInvalidArgumentsForSelector [

	| aModel aClass |
	aModel := RBNamespace new. 
	aModel defineClass: 'Object << #FOOBAR
				slots: { #asdf };
				package: #''Refactory-Test data'''.
	aClass := aModel classNamed: #FOOBAR.
	aClass compile: 'eiffel: tower ^ self ' classified: 'dummy'.
	
	self 
		assert: (RBCondition invalidArgumentNamesForSelector: #eiffel: in: aModel) asArray
		equals: #(#asdf #tower).
	
]

{ #category : #tests }
RBConditionTest >> testIsClassDefinedInModel [

	| aModel aClass |
	aModel := RBNamespace new. 
	aModel defineClass: 'Object << #FOOBAR
				slots: { #asdf };
				package: #''Refactory-Test data'''.
	aClass := aModel classNamed: #FOOBAR.
	self assert: (RBCondition isClass: aClass definedIn: aModel ) check
	
]

{ #category : #tests }
RBConditionTest >> testIsClassNamedDefinedIn [

	| aModel |
	aModel := RBNamespace new. 
	aModel defineClass: 'Object << #FOOBAR
				slots: { #asdf };
				package: #''Refactory-Test data'''.
	self assert: (RBCondition isClassNamed: #FOOBAR definedIn: aModel ) check
	
]

{ #category : #tests }
RBConditionTest >> testIsClassNamedDefinedInNot [

	| aModel |
	aModel := RBNamespace new. 
	aModel defineClass: 'Object << #FOOBAR
				slots: { #asdf };
				package: #''Refactory-Test data'''.
	self deny: (RBCondition isClassNamed: #FOOBAR2 definedIn: aModel ) check
	
]

{ #category : #tests }
RBConditionTest >> testTrue [
	
	self assert: RBCondition true check.
	self deny: RBCondition true not check.
]

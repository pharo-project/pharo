Class {
	#name : #RBConditionTest,
	#superclass : #TestCase,
	#category : #'Refactoring-Tests-Core'
}

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
RBConditionTest >> testCheckValidMethodName [
	"Usually used to validate input."
	self assert: (RBCondition checkMethodName: 'foo').
	self assert: (RBCondition checkMethodName: #foo).

	self assert: (RBCondition checkMethodName: #+)
]

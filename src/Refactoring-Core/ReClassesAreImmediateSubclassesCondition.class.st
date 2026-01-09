Class {
	#name : 'ReClassesAreImmediateSubclassesCondition',
	#superclass : 'ReClassesCondition',
	#instVars : [
		'superclass'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReClassesAreImmediateSubclassesCondition >> superclass: aRBClass [

	superclass := aRBClass 
]

{ #category : 'accessing' }
ReClassesAreImmediateSubclassesCondition >> violators [

	^ violators ifNil: [
			  violators := self candidates reject: [ :each |
				               (self model classObjectFor: each) superclass = (self model classObjectFor: superclass) ] ]
]

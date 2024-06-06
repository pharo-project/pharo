Class {
	#name : 'ReReifiedCondition',
	#superclass : 'RBCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'as yet unclassified' }
ReReifiedCondition class >> strategyChoiceClass [
	^ ReBrowseSubclassSuperSendsChoice 
]

{ #category : 'accessing' }
ReReifiedCondition >> errorString [

	^ String streamContents: [ :aStream | self violationMessageOn: aStream ]
]

{ #category : 'displaying' }
ReReifiedCondition >> violationMessageOn: aStream [

	self subclassResponsibility 
]

{ #category : 'accessing' }
ReReifiedCondition >> violators [

	^ self subclassResponsibility
]

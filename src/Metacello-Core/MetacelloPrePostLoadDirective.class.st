Class {
	#name : 'MetacelloPrePostLoadDirective',
	#superclass : 'MetacelloDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'actions' }
MetacelloPrePostLoadDirective >> evaluateSupplyingAnswers: loadBlock [

	| answers |
	(answers := self spec answers) notEmpty
		ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
		ifFalse: [ loadBlock value]
]

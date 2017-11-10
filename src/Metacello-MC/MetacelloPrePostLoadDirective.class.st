Class {
	#name : #MetacelloPrePostLoadDirective,
	#superclass : #MetacelloDirective,
	#category : #Metacello-MC-Directives
}

{ #category : #actions }
MetacelloPrePostLoadDirective >> evaluateSupplyingAnswers: loadBlock [

	| answers |
	(answers := self spec answers) notEmpty
		ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
		ifFalse: [ loadBlock value]
]

{ #category : #enumerating }
MetacelloPrePostLoadDirective >> prepostLoadDirectivesDo: aBlock [

	aBlock value: self
]

{ #category : #enumerating }
MetacelloPrePostLoadDirective >> prepostLoadDo: aBlock [

	aBlock value: self
]

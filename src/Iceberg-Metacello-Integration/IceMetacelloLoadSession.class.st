"
I'm a metacello execution sesssion. 
I will be active during the context of a ""metacello load"" command, and it will be shared between all dependent baselines/configurations to be traversed in that execution.

Tipically, I will be used to keep some state needed during the full execution, like a choice of what whas selected when conflicting or upgraded versions acts.  
"
Class {
	#name : #IceMetacelloLoadSession,
	#superclass : #Object,
	#instVars : [
		'choices'
	],
	#category : #'Iceberg-Metacello-Integration'
}

{ #category : #'as yet unclassified' }
IceMetacelloLoadSession >> initialize [

	super initialize.
	choices := Set new
]

{ #category : #'as yet unclassified' }
IceMetacelloLoadSession >> recordChoice: anIceMetacelloLoadChoice [
	
	^ choices add: anIceMetacelloLoadChoice
]

{ #category : #'as yet unclassified' }
IceMetacelloLoadSession >> recordChoiceProject: projectName optionA: optionA optionB: optionB ifAbsentAnswer: aBlock [
	
	^ choices 
		detect: [ :each | 
			(each projectName = projectName)
			and: [ each isChoiceBetween: optionA and: optionB ] ]
		ifFound: [ :choice | choice choosen ]
		ifNone: [ | choosen |
			choosen := aBlock value.
			choosen ifNotNil: [  
				self recordChoice: (IceMetacelloLoadChoice new 
					projectName: projectName;
					optionA: optionA;
					optionB: optionB;
					choosen: choosen;
					yourself) ].
			choosen ]
]

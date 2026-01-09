Class {
	#name : 'ReIsValidPackageName',
	#superclass : 'RePackageNameCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
ReIsValidPackageName >> violators [

	^ (packageName isSymbol and: [ packageName isNotEmpty ])
		  ifTrue: [ #(  ) ]
		  ifFalse: [ self candidates ]
]

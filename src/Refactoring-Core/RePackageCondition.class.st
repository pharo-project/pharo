Class {
	#name : 'RePackageCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'violator',
		'packageOrganizer'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing' }
RePackageCondition >> packageOrganizer [
    ^ packageOrganizer
]

{ #category : 'accessing' }
RePackageCondition >> packageOrganizer: anOrganizer [
    packageOrganizer := anOrganizer
]

{ #category : 'accessing' }
RePackageCondition >> violators [
    ^ self check
        ifFalse: [ { violator } ]
        ifTrue: [ #() ]
]

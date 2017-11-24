Class {
	#name : #MetacelloMCPartiallyLoadedStatus,
	#superclass : #Object,
	#instVars : [
		'hasNoPackage',
		'hasNoProject',
		'aProjectIsLoaded',
		'aPackageIsLoaded',
		'aLoadedProjectIsExact',
		'aLoadedPackageIsExact',
		'aLoadedProjectIsCurrent',
		'aLoadedPackageIsCurrent',
		'aLoadedProjectIsNotCurrent',
		'aLoadedPackageIsNotCurrent',
		'aProjectNotLoaded',
		'aPackageNotLoaded',
		'vrsnStatus',
		'abort'
	],
	#category : #'Metacello-MC-Specs'
}

{ #category : #'instance creation' }
MetacelloMCPartiallyLoadedStatus class >> new [
	^ self basicNew initialize
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedPackageIsCurrent [
	^ aLoadedPackageIsCurrent
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedPackageIsCurrent: aBoolean [
	aLoadedPackageIsCurrent := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedPackageIsExact [
	^ aLoadedPackageIsExact
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedPackageIsExact: aBoolean [
	aLoadedPackageIsExact := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedPackageIsNotCurrent [
	^ aLoadedPackageIsNotCurrent
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedPackageIsNotCurrent: aBoolean [
	aLoadedPackageIsNotCurrent := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedProjectIsCurrent: aBoolean [
	aLoadedProjectIsCurrent := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedProjectIsExact [
	^ aLoadedProjectIsExact
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedProjectIsExact: aBoolean [
	aLoadedProjectIsExact := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedProjectIsNotCurrent [
	^ aLoadedProjectIsNotCurrent
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aLoadedProjectIsNotCurrent: aBoolean [
	aLoadedProjectIsNotCurrent := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aPackageIsLoaded [
	^aPackageIsLoaded
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aPackageIsLoaded: aBoolean [
	aPackageIsLoaded := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aPackageNotLoaded [
	^aPackageNotLoaded
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aPackageNotLoaded: aBoolean [
	aPackageNotLoaded := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aProjectIsLoaded [
	^aProjectIsLoaded
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aProjectIsLoaded: aBoolean [
	aProjectIsLoaded := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aProjectNotLoaded [
	^aProjectNotLoaded
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> aProjectNotLoaded: aBoolean [
	aProjectNotLoaded := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> abort [
	^ abort
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> abort: aBoolean [
	abort := aBoolean
]

{ #category : #evaulation }
MetacelloMCPartiallyLoadedStatus >> evaluateStatus: validStatusList [
	^self abort
		ifTrue: [ false ]
		ifFalse: [ 
			(self hasNoProject or: [ self vrsnStatus isEmpty ])
				ifTrue: [ true ]
				ifFalse: [ 
					| valid |
					valid := true.
					vrsnStatus
						do: [ :status | 
							(validStatusList includes: status)
								ifFalse: [ valid := false ] ].
					valid ] ]
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> hasNoPackage [
	^ hasNoPackage
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> hasNoPackage: aBoolean [
	hasNoPackage := aBoolean
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> hasNoProject [
	^ hasNoProject
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> hasNoProject: aBoolean [
	hasNoProject := aBoolean
]

{ #category : #initialization }
MetacelloMCPartiallyLoadedStatus >> initialize [
	super initialize.
	hasNoPackage := hasNoProject := true.
	aProjectIsLoaded := aPackageIsLoaded := false.
	aLoadedProjectIsExact := aLoadedPackageIsExact := false.
	aLoadedProjectIsCurrent := aLoadedPackageIsCurrent := false.
	aLoadedProjectIsNotCurrent := aLoadedPackageIsNotCurrent := false.
	aProjectNotLoaded := aPackageNotLoaded := false.
	vrsnStatus := Set new.
	abort := false
]

{ #category : #testing }
MetacelloMCPartiallyLoadedStatus >> isAllLoadedToSpec: matchBlock [
        "all projects and packages are loaded and match specification"

	(self evaluateStatus: #(#allLoadedToSpec))
		ifTrue: [ 
			((hasNoPackage
				or: [ 
					aPackageIsLoaded & aPackageNotLoaded not & aLoadedPackageIsExact & aLoadedPackageIsNotCurrent not
						& aLoadedPackageIsCurrent not ])
				and: [ 
					hasNoProject
						or: [ 
							aProjectIsLoaded & aProjectNotLoaded not & aLoadedProjectIsExact & aLoadedProjectIsNotCurrent not
								& aLoadedProjectIsCurrent not ] ])
				ifTrue: [ matchBlock value: #allLoadedToSpec ] ]
]

{ #category : #testing }
MetacelloMCPartiallyLoadedStatus >> isLoadedMatchConstraints: matchBlock [
	"all loaded projects and packages match constraints (at least one package loaded)"

	| matchStatus |
	matchStatus := #loadedMatchConstraints.
	(self evaluateStatus: #(#allLoadedToSpec #loadedToSpec #loadedMatchConstraints))
		ifTrue: [ 
			aPackageIsLoaded & aProjectIsLoaded
				ifTrue: [ 
					(aLoadedPackageIsExact | aLoadedPackageIsCurrent & aLoadedPackageIsNotCurrent not
						and: [ aLoadedProjectIsExact | aLoadedProjectIsCurrent & aLoadedProjectIsNotCurrent not ])
						ifTrue: [ matchBlock value: matchStatus ] ]
				ifFalse: [ 
					aPackageIsLoaded
						ifTrue: [ 
							aLoadedPackageIsExact | aLoadedPackageIsCurrent & aLoadedPackageIsNotCurrent not
								ifTrue: [ matchBlock value: matchStatus ] ]
						ifFalse: [ 
							hasNoPackage & (aLoadedProjectIsExact | aLoadedProjectIsCurrent) & aLoadedProjectIsNotCurrent not
								ifTrue: [ matchBlock value: matchStatus ] ] ] ]
]

{ #category : #testing }
MetacelloMCPartiallyLoadedStatus >> isLoadedToSpec: matchBlock [
	"all loaded projects and packages match specifications (at least one package loaded)"

	| matchStatus |
	matchStatus := #loadedToSpec.
	(self evaluateStatus: #(#allLoadedToSpec #loadedToSpec))
		ifTrue: [ 
			aPackageIsLoaded & aProjectIsLoaded
				ifTrue: [ 
					(aLoadedPackageIsExact & aLoadedPackageIsCurrent not & aLoadedPackageIsNotCurrent not
						and: [ aLoadedProjectIsExact & aLoadedProjectIsCurrent not & aLoadedProjectIsNotCurrent not ])
						ifTrue: [ matchBlock value: matchStatus ] ]
				ifFalse: [ 
					aPackageIsLoaded
						ifTrue: [ 
							aLoadedPackageIsExact & aLoadedPackageIsCurrent not & aLoadedPackageIsNotCurrent not
								ifTrue: [ matchBlock value: matchStatus ] ]
						ifFalse: [ 
							hasNoPackage & aLoadedProjectIsExact & aLoadedProjectIsCurrent not & aLoadedProjectIsNotCurrent not
								ifTrue: [ matchBlock value: matchStatus ] ] ] ]
]

{ #category : #testing }
MetacelloMCPartiallyLoadedStatus >> isSomethingLoaded: somethingLoadedBlock [
	"at least one package loaded"

	(self evaluateStatus: #(#allLoadedToSpec #loadedToSpec #loadedMatchConstraints #somethingLoaded))
		ifTrue: [ 
			aPackageIsLoaded
				ifTrue: [ somethingLoadedBlock value: #somethingLoaded ] ]
]

{ #category : #accessing }
MetacelloMCPartiallyLoadedStatus >> vrsnStatus [
	^vrsnStatus
]

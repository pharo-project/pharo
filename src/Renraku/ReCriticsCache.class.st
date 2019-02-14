"
Same as CriticsCache but with tweaks for Renraku model 
"
Class {
	#name : #ReCriticsCache,
	#superclass : #CriticsCache,
	#category : #'Renraku-CriticBrowser'
}

{ #category : #initialization }
ReCriticsCache >> initCache [


	checker rule do: [ :rule | 
		falsePositives at: rule put: (IdentitySet newFrom: (checker falsePositiveOf: rule)).
		toDos at: rule put: (IdentitySet newFrom: (checker toDoOf: rule)).
		critics at: rule put: (IdentitySet newFrom: (checker criticsOf: rule)) 
	].
	packages do: [ :package |	
		falsePositiveRules at: package put: (checker rejectRulesOf: package) asSet.
		falsePositiveClasses addAll: (checker rejectClassesOf: package) asIdentitySet.
	]
]

{ #category : #private }
ReCriticsCache >> logFalsePositiveInManifest [
	| removeFp addFp |
	falsePositives
		keysAndValuesDo: [ :rule :criticss | 
			removeFp := (checker falsePositiveOf: rule) \ criticss.
			removeFp := removeFp collect: [ :c | c sourceAnchor entity ].
			addFp := criticss \ (checker falsePositiveOf: rule).
			addFp := addFp collect: [ :c | c sourceAnchor entity ].			
			(removeFp groupedBy: [ :each | self builderManifestClass of: each ])
				keysAndValuesDo: [ :manifestBuilder :value | self removeAllCriticToFalsePositive: value forRule: rule on: manifestBuilder ].
			(addFp groupedBy: [ :each | self builderManifestClass of: each ])
				keysAndValuesDo: [ :manifestBuilder :value | 
					(manifestBuilder rejectRules includes: rule class uniqueIdentifierName)
						ifFalse: [ self addAllCriticToFalsePositive: value forRule: rule on: manifestBuilder ] ] ]
]

{ #category : #private }
ReCriticsCache >> logToDosInManifest [
	| removeFp addFp |
	toDos
		keysAndValuesDo: [ :rule :criticss | 
			removeFp := (checker toDoOf: rule) \ criticss.
			removeFp := removeFp collect: [ :c | c sourceAnchor entity ].
			addFp := criticss \ (checker toDoOf: rule).
			addFp := addFp collect: [ :c | c sourceAnchor entity ].	
			(removeFp groupedBy: [ :each | self builderManifestClass of: each ])
				keysAndValuesDo: [ :manifestBuilder :value | self removeAllCriticToToDo: value forRule: rule on: manifestBuilder ].
			(addFp groupedBy: [ :each | self builderManifestClass of: each ])
				keysAndValuesDo: [ :manifestBuilder :value | 
					(manifestBuilder rejectRules includes: rule class uniqueIdentifierName)
						ifFalse: [ self addAllCriticToToDo: value forRule: rule on: manifestBuilder ] ] ]
]

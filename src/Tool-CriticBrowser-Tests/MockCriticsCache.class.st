Class {
	#name : #MockCriticsCache,
	#superclass : #CriticsCache,
	#category : #'Tool-CriticBrowser-Tests-MockCriticCache'
}

{ #category : #accessing }
MockCriticsCache >> critics [
	^ critics
]

{ #category : #accessing }
MockCriticsCache >> falsePositiveClass [
	^ falsePositiveClasses
]

{ #category : #accessing }
MockCriticsCache >> falsePositives [
	^ falsePositives 
]

{ #category : #initialization }
MockCriticsCache >> initCache [
	checker rule
		do: [ :rule | 
			falsePositives
				at: rule
				put: (IdentitySet newFrom: (checker falsePositiveOf: rule)).
			toDos at: rule put: (IdentitySet newFrom: (checker toDoOf: rule)).
			critics
				at: rule
				put: (IdentitySet newFrom: (checker criticsOf: rule)) ].
	packages
		do: [ :package | 
			falsePositiveRules
				at: package
				put: (checker rejectRulesOf: package) asSet.
			falsePositiveClasses
				addAll: (checker rejectClassesOf: package) asIdentitySet ]
]

{ #category : #private }
MockCriticsCache >> updateBrowser [

	"do nothing"
]

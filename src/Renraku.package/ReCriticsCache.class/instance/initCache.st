initialize-release
initCache


	checker rule do: [ :rule | 
		falsePositives at: rule put: (IdentitySet newFrom: (checker falsePositiveOf: rule)).
		toDos at: rule put: (IdentitySet newFrom: (checker toDoOf: rule)).
		critics at: rule put: (IdentitySet newFrom: (checker criticsOf: rule)) 
	].
	packages do: [ :package |	
		falsePositiveRules at: package put: (checker rejectRulesOf: package) asSet.
		falsePositiveClasses addAll: (checker rejectClassesOf: package) asIdentitySet.
	]
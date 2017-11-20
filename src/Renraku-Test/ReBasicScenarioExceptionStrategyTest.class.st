Class {
	#name : #ReBasicScenarioExceptionStrategyTest,
	#superclass : #ReExceptionStrategyBaseTest,
	#category : #Renraku-Test
}

{ #category : #tests }
ReBasicScenarioExceptionStrategyTest >> testIgnore [
	| critiques |

	ReExceptionStrategy current: ReIgnoreExceptionStrategy.
	
	self
		shouldnt: [
			critiques := self validationScenario  ]
		raise: MyTestError.
		
	self assert: critiques isEmpty
]

{ #category : #tests }
ReBasicScenarioExceptionStrategyTest >> testLog [
	| critiques |

	ReExceptionStrategy current: ReLogExceptionStrategy.
	
	self
		shouldnt: [
			critiques := self validationScenario  ]
		raise: MyTestError.
		
	self assert: critiques isNotEmpty.
	self assert: (
		critiques anySatisfy: [ :crit |
		 (crit isKindOf: ReExceptionProperty) and: [ 
			crit message includesSubstring: self errorMessage ] ])
]

{ #category : #tests }
ReBasicScenarioExceptionStrategyTest >> testPass [

	ReExceptionStrategy current: RePassExceptionStrategy.
	
	self
		should: [ self validationScenario  ]
		raise: MyTestError
]

{ #category : #helpers }
ReBasicScenarioExceptionStrategyTest >> validationScenario [
	| critiques |
	
	critiques := Set new.

	[ MyTestError signal: self errorMessage ]
		on: Error
		do: [ :er |
			ReExceptionStrategy current
				handle: er
				about: self
				forProppertiesDo: [ :prop |
					critiques add: prop ] ].
			
	^ critiques
	
]

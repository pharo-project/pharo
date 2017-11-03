Class {
	#name : #ReExceptionStrategyTest,
	#superclass : #TestCase,
	#category : #Renraku-Test
}

{ #category : #tests }
ReExceptionStrategyTest >> testSettingDescription [

	ReExceptionStrategy allSubclassesDo: [ :strategy |
		self assert: (
			ReExceptionStrategy settingDescription
				includesSubstring: strategy strategyDescription) ]
]

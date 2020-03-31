Class {
	#name : #ReExceptionStrategyTest,
	#superclass : #TestCase,
	#category : #'Renraku-Tests'
}

{ #category : #tests }
ReExceptionStrategyTest >> testSettingDescription [

	ReExceptionStrategy allSubclassesDo: [ :strategy |
		self assert: (
			ReExceptionStrategy settingDescription
				includesSubstring: strategy strategyDescription) ]
]

Class {
	#name : 'ReExceptionStrategyTest',
	#superclass : 'TestCase',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'tests' }
ReExceptionStrategyTest >> testSettingDescription [

	ReExceptionStrategy allSubclassesDo: [ :strategy |
		self assert: (
			ReExceptionStrategy settingDescription
				includesSubstring: strategy strategyDescription) ]
]

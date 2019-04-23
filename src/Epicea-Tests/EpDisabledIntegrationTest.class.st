Class {
	#name : #EpDisabledIntegrationTest,
	#superclass : #EpMonitorIntegrationTest,
	#category : #'Epicea-Tests-Integration'
}

{ #category : #tests }
EpDisabledIntegrationTest >> testDisableDuring [
	monitor enable.
	self assert: monitor isEnabled.
	monitor disableDuring: [ self deny: monitor isEnabled ].
	self assert: monitor isEnabled.
	monitor disable.
	self deny: monitor isEnabled.
	monitor disableDuring: [ self deny: monitor isEnabled ].
	self deny: monitor isEnabled.
]

{ #category : #tests }
EpDisabledIntegrationTest >> testDisableDuringWithInteruption [
	monitor enable.
	self assert: monitor isEnabled.
	[ monitor
		disableDuring: [ self deny: monitor isEnabled.
			self error ] ]
		on: Error
		do: [ "We just want to test the case with an error." ].
	self assert: monitor isEnabled
]

{ #category : #tests }
EpDisabledIntegrationTest >> testEnable [
	monitor enable.
	
	self assert: (self countLogEventsWith: EpClassAddition) equals: 0.
	
	classFactory newClass.

	self assert: (self countLogEventsWith: EpClassAddition) equals: 1.

]

{ #category : #tests }
EpDisabledIntegrationTest >> testIsEnabled [
	self deny: monitor isEnabled.
	monitor enable.
	self assert: monitor isEnabled.
	monitor disable.
	self deny: monitor isEnabled.

]

{ #category : #tests }
EpDisabledIntegrationTest >> testWhenDisabledShouldNotLog [
	classFactory newClass.

	self assert: (self countLogEventsWith: EpClassAddition) equals: 0
]

{ #category : #tests }
EpDisabledIntegrationTest >> testWhenEnabledShouldLog [
	monitor enable.

	classFactory newClass.

	self assert: (self countLogEventsWith: EpClassAddition) equals: 1
]

{ #category : #tests }
EpDisabledIntegrationTest >> testWhenEnabledTwiceShouldNotDuplicateEntries [
	2 timesRepeat: [ monitor enable ].

	classFactory newClass.

	self assert: (self countLogEventsWith: EpClassAddition) equals: 1
]

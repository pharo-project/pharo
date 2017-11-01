"
An AbstractStoredSettingTest is a test class for testing the behavior of AbstractStoredSetting
"
Class {
	#name : #AbstractStoredSettingTest,
	#superclass : #TestCase,
	#category : 'System-Settings-Tests'
}

{ #category : #tests }
AbstractStoredSettingTest >> testEqual [
	AbstractStoredSetting allSubclassesDo: [ :aClass |
		self assert: aClass new equals: aClass new ]
]

{ #category : #tests }
AbstractStoredSettingTest >> testHash [
	AbstractStoredSetting allSubclassesDo: [ :aClass |
		self assert: aClass new hash equals: aClass new hash ]
]

{ #category : #tests }
AbstractStoredSettingTest >> testPrintOn [
	AbstractStoredSetting allSubclassesDo: [ :aClass |
		String streamContents: [ :aStream | aClass new printOn: aStream ] ]
]

{ #category : #tests }
AbstractStoredSettingTest >> testPrintOn2 [
	AbstractStoredSetting allSubclassesDo: [ :aClass | | aString |
		aString := String streamContents: [ :aStream | | anInstance |
			anInstance := aClass new.
			anInstance settingNodeIdentifier: #'print-on-identifier'.
			anInstance printOn: aStream ].
		self assert: (aString includesSubstring: #'print-on-identifier') ]
]

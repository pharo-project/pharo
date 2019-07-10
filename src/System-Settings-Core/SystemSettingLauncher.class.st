"
Used to launch a setting
"
Class {
	#name : #SystemSettingLauncher,
	#superclass : #PragmaSetting,
	#instVars : [
		'script'
	],
	#category : #'System-Settings-Core-Utilities'
}

{ #category : #accessing }
SystemSettingLauncher >> defaultIcon [
	^ self iconNamed: #smallDoItIcon
]

{ #category : #accessing }
SystemSettingLauncher >> inputWidget [
	^ (self theme
		newButtonIn: self currentWorld
		for: self
		getState: nil
		action: #launch
		arguments: {}
		getEnabled: nil
		getLabel: nil
		help: self description)
		label:
			(self theme
				newRowIn: self currentWorld
				for:
					{(AlphaImageMorph new
						image: (self iconNamed: #smallRedoIcon)).
					(self theme buttonLabelForText: 'Launch' translated)});
		yourself
]

{ #category : #accessing }
SystemSettingLauncher >> launch [
	self script 
			ifNotNil: [:s | s isSymbol 
				ifTrue: [s numArgs = 0 
					ifTrue: [self settingReceiver perform: s]
					ifFalse: [self settingReceiver perform: s with: self]]
				ifFalse: [s numArgs = 0 
					ifTrue: [s value]
					ifFalse: [s value: self]]]
]

{ #category : #accessing }
SystemSettingLauncher >> script [
	^ script ifNil: [script := []]
]

{ #category : #accessing }
SystemSettingLauncher >> script: aValuableOrSymbol [
	script := aValuableOrSymbol
]

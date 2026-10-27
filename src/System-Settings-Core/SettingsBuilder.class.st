"
I build the settings (basic settings and groups) from a list of pragmas.
These pragma identify methods with settings description that I can execute, passing myself as argument, to build the settings.

You can use me as follows:
```
builder := SettingsBuilder forPragmaNamed: #settings_test_data.
builder settings
```

When accessing settings, if not available, settings will be build.
"
Class {
	#name : 'SettingsBuilder',
	#superclass : 'Object',
	#instVars : [
		'pragmas',
		'settings',
		'parents',
		'currentPragma'
	],
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'instance creation' }
SettingsBuilder class >> forPragmaNamed: aSymbol [
	
	| collector |
	collector := PragmaCollector
		selectors: (Array with: aSymbol)
		filter: [ :pragma | pragma methodClass isClassSide ].
		
	^ self forPragmas: collector pragmas
]

{ #category : 'instance creation' }
SettingsBuilder class >> forPragmas: aListOfPragmas [
	
	^ self new
		pragmas: aListOfPragmas;
		yourself
]

{ #category : 'private - building' }
SettingsBuilder >> addSettingNamed: aSymbol class: aClass [

	| setting |
	setting := aClass new
		           name: aSymbol;
		           yourself.
	self setParentIfAnyTo: setting.
	setting declaration: currentPragma.
	settings add: setting.
	^ setting
]

{ #category : 'private - building' }
SettingsBuilder >> buildPragma: aPragma [

	currentPragma := aPragma.
	ActiveSettingBuilder value: self during: [
			aPragma methodClass instanceSide
				perform: aPragma methodSelector
				with: self ].
	currentPragma := nil
]

{ #category : 'private - building' }
SettingsBuilder >> buildSettings [
	
	pragmas do: [ :pragma | self buildPragma: pragma ].
	self fixGroups
]

{ #category : 'building' }
SettingsBuilder >> button: aString [

	^ self addSettingNamed: aString asSymbol class: ActionSetting
]

{ #category : 'private - building' }
SettingsBuilder >> fixGroups [

	settings 
		select: [ :setting | setting hasParent not and: [ setting parentName notNil ] ]
		thenDo: [ :setting | setting parentGroup: (settings detect: [ :s | s name = setting parentName ]) ]
]

{ #category : 'building' }
SettingsBuilder >> group: aString [

	^ self addSettingNamed: aString asSymbol class: SettingsGroup
]

{ #category : 'initialization' }
SettingsBuilder >> initialize [ 
	
	super initialize.
	settings := OrderedCollection new.
	parents := Stack new
]

{ #category : 'private - building' }
SettingsBuilder >> parent: aSetting while: aBlock [

	parents push: aSetting.
	[ aBlock value ] ensure: [ parents pop ]
]

{ #category : 'building' }
SettingsBuilder >> pickOne: aString [

	^ self addSettingNamed: aString asSymbol class: PickOneSetting
]

{ #category : 'initialization' }
SettingsBuilder >> pragmas: aListOfPragmas [ 
	
	pragmas := aListOfPragmas
]

{ #category : 'building' }
SettingsBuilder >> range: aString [

	^ self addSettingNamed: aString asSymbol class: RangeSetting
]

{ #category : 'private - building' }
SettingsBuilder >> setParentIfAnyTo: aSetting [

	parents ifNotEmpty: [
			aSetting
				parentName: parents top;
				parentName: parents top name ]
]

{ #category : 'building' }
SettingsBuilder >> setting: aString [

	^ self addSettingNamed: aString asSymbol class: Setting
]

{ #category : 'accessing' }
SettingsBuilder >> settings [
	
	^ settings ifEmpty: [ self buildSettings. settings ]
]

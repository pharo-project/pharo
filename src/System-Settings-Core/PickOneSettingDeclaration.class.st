"
A CheckListSettingDeclaration is a setting for which the value domain is a list. Each element of the list is an instance of FixedSettingValue. If domainValues is set, then the list of valid values is constant (initialized at declaration time). Instead, if getter is set, then the setting list is always dynamically computed. See SettingManager comment for more explanations. 

Instance Variables

"
Class {
	#name : #PickOneSettingDeclaration,
	#superclass : #MultipleSettingDeclaration,
	#category : #'System-Settings-Core-Base'
}

{ #category : #'user interface' }
PickOneSettingDeclaration >> chooseValue [
	| chosen |
	chosen := UIManager default
				chooseFrom: self domainValuesLabels
				values: self domainValues
				title: self label translated, ' choices' translated.
	chosen
		ifNotNil: [self realValue: chosen realValue]
]

{ #category : #'user interface' }
PickOneSettingDeclaration >> content [
	^ self domainValues
		detect: [ :setting | setting realValue = self realValue ]
		ifFound: [ :setting | setting name ]
		ifNone: [ 'nil' ]
]

{ #category : #'user interface' }
PickOneSettingDeclaration >> defaultValue [
	^ self default value ifNil: [self domainValues first realValue]

]

{ #category : #'user interface' }
PickOneSettingDeclaration >> index [
	self realValue ifNil: [self realValue: self default].
	^ self domainValues indexOf: (self domainValues detect: [:dv | dv realValue = self realValue] ifNone: [^ 0])
]

{ #category : #'user interface' }
PickOneSettingDeclaration >> index: anInteger [
	self realValue: (self domainValues at: anInteger) realValue.

]

{ #category : #'user interface' }
PickOneSettingDeclaration >> inputWidget [
	| widget row |
	row := self theme newRowIn: self currentWorld for: {
				widget := (self theme 
						newDropListIn: self currentWorld
						for: self
						list: #domainValuesLabels
						getSelected: #index
						setSelected: #index:
						getEnabled: #enabled
						useIndex: true
						help: self description)
						extent: 1 @ 30}.
	widget hResizing: #rigid.
	widget width: (self maxNameWidthForFont: widget font) + 50.
	^ row
]

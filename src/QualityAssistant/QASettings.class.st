"
I hold settings for quality assistant
"
Class {
	#name : #QASettings,
	#superclass : #Object,
	#category : #QualityAssistant
}

{ #category : #settings-accessing }
QASettings class >> nautilusPlugin [
	
	^ QANautilusPlugin isInstalled
]

{ #category : #settings-accessing }
QASettings class >> nautilusPlugin: aBoolean [

	aBoolean
		ifTrue:  [ QANautilusPlugin install ]
		ifFalse: [ QANautilusPlugin uninstall ]
]

{ #category : #settings-definition }
QASettings class >> qaGroupSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #qualityAssistant)
		label: 'QualityAssistant';
		parent: #codeBrowsing.
]

{ #category : #settings-definition }
QASettings class >> qaNautilusPlugginSettingOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #nautilusPlugin)
		order: 4;
		target: self;
		label: 'Nautilus Plugin';
		description: 'Enable Nautilus plugin which shows a group with critiques and allows to preview them.';
		parent: #qualityAssistant.
]

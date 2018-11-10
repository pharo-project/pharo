Class {
	#name : #IceTipCredentialsSettings,
	#superclass : #Object,
	#category : #'Iceberg-TipUI-Settings'
}

{ #category : #settings }
IceTipCredentialsSettings class >> editButtonAction [
	IceTipCredentialsStoreBrowser new openWithSpec.
]

{ #category : #settings }
IceTipCredentialsSettings class >> editButtonLabel [
	^ 'Edit'
]

{ #category : #settings }
IceTipCredentialsSettings class >> editButtonState [
	^ true
]

{ #category : #settings }
IceTipCredentialsSettings class >> settingsOn: aBuilder [
	<systemsettings>
		
	(aBuilder group: #EditCredentials)
		parent: #icebergCredentials;
		noOrdering;
		target: self;
		label: 'Edit Credentials';
		dialog: [ 	
				PluggableButtonMorph
					on: self
					getState: nil
					action: #editButtonAction
					label: #editButtonLabel ]


]

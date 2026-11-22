"
I am the ""Extras"" sub-menu of the context menu of inherited method groups and of variables.
"
Class {
	#name : 'ClyMethodGroupExtraMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'Calypso-SystemTools-FullBrowser',
	#package : 'Calypso-SystemTools-FullBrowser'
}

{ #category : 'accessing' }
ClyMethodGroupExtraMenuGroup >> iconName [

	^ #smallRedo
]

{ #category : 'accessing' }
ClyMethodGroupExtraMenuGroup >> name [

	^ 'Extras'
]

{ #category : 'accessing' }
ClyMethodGroupExtraMenuGroup >> order [

	^ 10200
]

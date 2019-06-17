Class {
	#name : #ClySUnitMethodMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-MenuGroups'
}

{ #category : #testing }
ClySUnitMethodMenuGroup >> isInlined [
	^ true
]

{ #category : #accessing }
ClySUnitMethodMenuGroup >> name [
	^ 'SUnit'
]

{ #category : #accessing }
ClySUnitMethodMenuGroup >> order [
	^ 1.5
]

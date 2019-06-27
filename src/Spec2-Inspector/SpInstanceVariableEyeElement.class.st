"
I am an eye element for instance variables.
"
Class {
	#name : #SpInstanceVariableEyeElement,
	#superclass : #SpAbstractEyeElement,
	#instVars : [
		'instVarName'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
SpInstanceVariableEyeElement class >> host: anObject instVarName: aString [
	^ (self host: anObject) instVarName: aString
]

{ #category : #comparing }
SpInstanceVariableEyeElement >> = anObject [
	^ super = anObject and: [ instVarName = anObject instVarName ]
]

{ #category : #menu }
SpInstanceVariableEyeElement >> accessorCode [
	^ self instVarName
]

{ #category : #menu }
SpInstanceVariableEyeElement >> customSubMenu: aMenu [
	
	aMenu addGroup: [ :aGroup | 
		aGroup 
			addItem: [:anItem | 
				anItem 
					name: 'List Methods Using "', self instVarName, '"';
					action: [ self referencesToInstanceVariable] ];
			addItem: [:anItem | 
				anItem 
					name: 'List Methods Storing into "', self instVarName, '"';
					action: [ self storingsIntoInstanceVariable].
			].
		]
]

{ #category : #comparing }
SpInstanceVariableEyeElement >> hash [
	^ super hash bitXor: instVarName hash
]

{ #category : #accessing }
SpInstanceVariableEyeElement >> instVarName [
	^ instVarName
]

{ #category : #accessing }
SpInstanceVariableEyeElement >> instVarName: aString [
	instVarName := aString
]

{ #category : #accessing }
SpInstanceVariableEyeElement >> label [
	^ self instVarName
]

{ #category : #action }
SpInstanceVariableEyeElement >> referencesToInstanceVariable [
	"Open a browser on all references to the selected instance variable, if that's what currently selected."

	^ self systemNavigation browseAllAccessesTo: self instVarName from: self hostClass
]

{ #category : #action }
SpInstanceVariableEyeElement >> save: aValue [
	self host instVarNamed: self instVarName put: aValue
]

{ #category : #action }
SpInstanceVariableEyeElement >> storingsIntoInstanceVariable [

	^ self systemNavigation browseAllStoresInto: self instVarName from: self hostClass
]

{ #category : #accessing }
SpInstanceVariableEyeElement >> value [
	^ self host readSlotNamed: self instVarName
]

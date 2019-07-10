"
I am an eye element for instance variables.
"
Class {
	#name : #InstanceVariableEyeElement,
	#superclass : #AbstractEyeElement,
	#instVars : [
		'instVarName'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
InstanceVariableEyeElement class >> host: anObject instVarName: aString [
	^ (self host: anObject) instVarName: aString
]

{ #category : #comparing }
InstanceVariableEyeElement >> = anObject [
	^ super = anObject and: [ instVarName = anObject instVarName ]
]

{ #category : #menu }
InstanceVariableEyeElement >> accessorCode [
	^ self instVarName
]

{ #category : #menu }
InstanceVariableEyeElement >> customSubMenu: aMenu [
	
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
InstanceVariableEyeElement >> hash [
	^ super hash bitXor: instVarName hash
]

{ #category : #accessing }
InstanceVariableEyeElement >> instVarName [
	^ instVarName
]

{ #category : #accessing }
InstanceVariableEyeElement >> instVarName: aString [
	instVarName := aString
]

{ #category : #accessing }
InstanceVariableEyeElement >> label [
	^ self instVarName
]

{ #category : #action }
InstanceVariableEyeElement >> referencesToInstanceVariable [
	"Open a browser on all references to the selected instance variable, if that's what currently selected."

	^ self systemNavigation browseAllAccessesTo: self instVarName from: self hostClass
]

{ #category : #action }
InstanceVariableEyeElement >> save: aValue [
	self host instVarNamed: self instVarName put: aValue
]

{ #category : #action }
InstanceVariableEyeElement >> storingsIntoInstanceVariable [

	^ self systemNavigation browseAllStoresInto: self instVarName from: self hostClass
]

{ #category : #accessing }
InstanceVariableEyeElement >> value [
	^ self host readSlotNamed: self instVarName
]

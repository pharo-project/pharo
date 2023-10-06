"
In case a user is using me to update the class side of a class only, I am here to provide a different API to the user. 

For example the user will be able to use #slots: instead of #classSlots:.
"
Class {
	#name : 'ShiftClassSideBuilder',
	#superclass : 'ShiftClassBuilder',
	#category : 'Shift-ClassBuilder',
	#package : 'Shift-ClassBuilder'
}

{ #category : 'class initialization' }
ShiftClassSideBuilder class >> initialize [

	self deprecatedAliases: #( #FluidClassSideClassBuilder #FluidClassSideTraitBuilder )
]

{ #category : 'accessing' }
ShiftClassSideBuilder >> slots: anArray [
	"Pay attention on the class side we do not have to have classSlots: but slots:"

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	self classSlots: anArray
]

{ #category : 'accessing' }
ShiftClassSideBuilder >> traits: aCollection [
	"Pay attention that on class side we want to write traits: and not classTraits:"

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	self classTraits: aCollection
]

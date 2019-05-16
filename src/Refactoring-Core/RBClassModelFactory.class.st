"
None of the classes inside this package should be directly accessed. They should be accessed via myself. I'm a class factory. 

I'm an important design point: I will let future evolution of the system to be experimented without the needs to change all the users. Then once the experiments and a good solution is found it may be the time to remove me and to think that I'm an overengineered solution. Right now I'm a change enabler. For example people can try to use Ring models to model RBmodel. 
"
Class {
	#name : #RBClassModelFactory,
	#superclass : #Object,
	#classVars : [
		'RBclass',
		'RBmetaclass',
		'RBmethod',
		'RBnamespace'
	],
	#category : #'Refactoring-Core-Model'
}

{ #category : #'factory access' }
RBClassModelFactory class >> rbClass [
 
	^ RBclass ifNil: [ RBclass := RBClass ]
]

{ #category : #'factory access' }
RBClassModelFactory class >> rbMetaclass [
 
	^ RBmetaclass ifNil: [ RBmetaclass := RBMetaclass ]
]

{ #category : #'factory access' }
RBClassModelFactory class >> rbMethod [
 
	^ RBmethod ifNil: [ RBmethod := RBMethod ]
]

{ #category : #'factory access' }
RBClassModelFactory class >> rbNamespace [
 
	^ RBnamespace ifNil: [ RBnamespace := RBNamespace ]
]

{ #category : #'factory customisation' }
RBClassModelFactory class >> setRBClass: aClass [

	RBclass := aClass
]

{ #category : #'factory customisation' }
RBClassModelFactory class >> setRBMetaclass: aClass [

	RBmetaclass := aClass
]

{ #category : #'factory customisation' }
RBClassModelFactory class >> setRBMethod: aClass [

	RBmethod := aClass
]

{ #category : #'factory customisation' }
RBClassModelFactory class >> setRBNamespace: aClass [
 
	RBnamespace := aClass
]

{ #category : #'factory access' }
RBClassModelFactory >> rbClass [ 

	^ self class rbClass
]

{ #category : #'factory access' }
RBClassModelFactory >> rbMetaclass [

	^ self class rbMetaclass
]

{ #category : #'factory access' }
RBClassModelFactory >> rbMethod [

	^ self class rbMethod
]

{ #category : #'factory access' }
RBClassModelFactory >> rbNamespace [

	^ self class rbNamespace
]

Class {
	#name : 'MCExtensionDefinition',
	#superclass : 'MCClassDefinition',
	#instVars : [
		'extendedClass'
	],
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'visiting' }
MCExtensionDefinition >> accept: aVisitor [

	^ aVisitor visitExtensionDefinition: self
]

{ #category : 'installing' }
MCExtensionDefinition >> createClassInEnvironment: anEnvironent [

	^ [
		  | theActualExtendedclass |
		  theActualExtendedclass := (anEnvironent ifNil: [ self class environment ]) at: extendedClass asSymbol.
		  self class classInstaller make: [ :aBuilder |
				  aBuilder
					  name: name;
					  extendedClass: theActualExtendedclass;
					  tag: self tagName;
					  package: self packageName;
					  comment: comment stamp: self commentStamp;
					  beExtension.
				  anEnvironent ifNotNil: [ aBuilder environment: anEnvironent ] ] ]
		  on: Warning , DuplicatedVariableError
		  do: [ :ex | ex resume ]
]

{ #category : 'accessing' }
MCExtensionDefinition >> extendedClass [

	^ extendedClass
]

{ #category : 'accessing' }
MCExtensionDefinition >> extendedClass: anObject [

	extendedClass := anObject
]

{ #category : 'accessing' }
MCExtensionDefinition >> isExtensionDefinition [

	^ true
]

{ #category : 'printing' }
MCExtensionDefinition >> printDefinitionOn: stream [

	stream
		nextPutAll: 'Extension << #' , self className;
		cr;
		tab;
		nextPutAll: 'extendedClass: #';
		nextPutAll: extendedClass;
		cr.

	stream
		tab;
		nextPutAll: 'package: ';
		store: self category asString
]

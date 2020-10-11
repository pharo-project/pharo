Class {
	#name : #LegacyClassDefinitionPrinter,
	#superclass : #ClassDefinitionPrinter,
	#category : #'Kernel-ClassDefinitionPrinter'
}

{ #category : #template }
LegacyClassDefinitionPrinter >> classDefinitionTemplateInPackage: aString [

	^ String streamContents: [ :s |
			s nextPutAll: 'Object subclass: #MyClass' ; crtab.
			s nextPutAll: 'instanceVariableNames: '''' '; crtab. 
			s nextPutAll: 'classVariableNames: '''''; crtab.
			s nextPutAll: 'poolDictionaries: '''''; crtab.
			s nextPutAll: 'category ''', aString, '''' ]
]

{ #category : #delegating }
LegacyClassDefinitionPrinter >> definitionString [
	^ forClass oldDefinition
]

{ #category : #delegating }
LegacyClassDefinitionPrinter >> expandedDefinitionString [
	"We do not support expansion on legacy syntax so we shortcut the double dispatch call."
	^ self definitionString
]

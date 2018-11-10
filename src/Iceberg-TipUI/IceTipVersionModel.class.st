"
I'm a model for IceLogVersion entries. 

- I provide log-specific entries.
- I can install a certain method version.
"
Class {
	#name : #IceTipVersionModel,
	#superclass : #IceTipCommitModel,
	#instVars : [
		'commitModels'
	],
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #private }
IceTipVersionModel >> basicInstall [
	| definition |
	
	definition := self entity definition.
	definition realClass 
		compile: definition sourceCode
		classified: definition category	
]

{ #category : #accessing }
IceTipVersionModel >> commit [
	^ self entity commit
]

{ #category : #actions }
IceTipVersionModel >> install [
	<noCache>

	IceTipStandardAction new 
		repository: self repositoryModel entity; 
		message: ('Installing {1} - {2}' format: { self entity definition. self name });
		onSuccessRepositoryModified;
		execute: [ self basicInstall ]
]

{ #category : #accessing }
IceTipVersionModel >> source [
	^ self entity source
]

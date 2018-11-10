Extension { #name : #MetacelloScriptApiExecutor }

{ #category : #'*Iceberg-Metacello-Integration' }
MetacelloScriptApiExecutor >> execute: statements [

	MetacelloPlatform current withMetacelloLoadSessionDo: [ 
		super execute: statements ]
]

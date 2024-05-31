"
A MCPoolImportDefinition represents a pool definition.
"
Class {
	#name : 'MCPoolImportDefinition',
	#superclass : 'MCVariableDefinition',
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'accessing' }
MCPoolImportDefinition class >> type [
	^ #pool
]

{ #category : 'testing' }
MCPoolImportDefinition >> isOrderDependend [
	^false
]

{ #category : 'testing' }
MCPoolImportDefinition >> isPoolImport [
	^ true
]

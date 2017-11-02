Class {
	#name : #MetacelloMCTestsAbstractConfig,
	#superclass : #ConfigurationOf,
	#category : 'Metacello-TestsMCResources'
}

{ #category : #accessing }
MetacelloMCTestsAbstractConfig >> project [

	| constructor |
	^project ifNil: [
		"Construct Metacello project"
		constructor := MetacelloVersionConstructor on: self.
		project := constructor project.
		project loader: MetacelloNullRecordingMCSpecLoader new.
		project].

]

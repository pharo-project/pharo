"
I am a shared pool used for styler tests.
"
Class {
	#name : 'SHRBMockSharedPoolStyler',
	#superclass : 'SharedPool',
	#classVars : [
		'PoolVar'
	],
	#category : 'Shout-Tests-Styling',
	#package : 'Shout-Tests',
	#tag : 'Styling'
}

{ #category : 'initialization' }
SHRBMockSharedPoolStyler >> initialize [
		<ignoreUnusedClassVariables: #( PoolVar )>
		super initialize.
]

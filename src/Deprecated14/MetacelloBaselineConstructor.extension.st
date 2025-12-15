Extension { #name : 'MetacelloBaselineConstructor' }

{ #category : '*Deprecated14' }
MetacelloBaselineConstructor >> author: aBlockOrString [

	self deprecated: 'The author metadata is not supported anymore. It was used with SmalltalkHub but not this information is already managed by git or other CVS.
	If you wish to keep this as a trace, you can leave the infos in a method comment.'
]

{ #category : '*Deprecated14' }
MetacelloBaselineConstructor >> blessing: aBlockOrString [

	self deprecated: 'The blessing metadata is not supported anymore.'
]

{ #category : '*Deprecated14' }
MetacelloBaselineConstructor >> configuration: aString with: aBlock [

	self error: 'Configurations are not supported anymore, only baseline can be used as dependencies.'
]

{ #category : '*Deprecated14' }
MetacelloBaselineConstructor >> description: aBlockOrString [

	self deprecated:
		'The description metadata is not supported anymore. It was used with SmalltalkHub but not this information is already managed by git or other CVS.
	If you wish to keep this as a trace, you can leave the infos in a method comment.'
]

{ #category : '*Deprecated14' }
MetacelloBaselineConstructor >> supplyingAnswers: aCollection [

	self deprecated: 'This option is not available anymore since multiple versions of Pharo.'
]

{ #category : '*Deprecated14' }
MetacelloBaselineConstructor >> timestamp: aBlockOrStringOrDateAndTime [

	self deprecated:
		'The timestamp metadata is not supported anymore. It was used with SmalltalkHub but not this information is already managed by git or other CVS.
	If you wish to keep this as a trace, you can leave the infos in a method comment.'
]

{ #category : '*Deprecated14' }
MetacelloBaselineConstructor >> versionString: anObject [

	self error:
		'Configurations are not supported anymore, only baseline can be used as dependencies and project managed with baseline have their version managed by the repositories.'
]

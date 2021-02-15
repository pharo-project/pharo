Class {
	#name : #StPharoDefaultIconProvider,
	#superclass : #SpPharoThemeIconProvider,
	#instVars : [
		'synonyms'
	],
	#category : #'NewTools-Core-Application'
}

{ #category : #accessing }
StPharoDefaultIconProvider >> iconNamed: aSymbol [

	^ super iconNamed: (synonyms 
		at: aSymbol 
		ifAbsent: [ aSymbol ])
]

{ #category : #'class initialization' }
StPharoDefaultIconProvider >> initialize [

	super initialize. 
	synonyms := Dictionary new.
	self initializeSynonyms
]

{ #category : #'class initialization' }
StPharoDefaultIconProvider >> initializeSynonyms [

	synonyms 
		at: #debuggerCopyStack 			put: #smallCopy;
		at: #debuggerDefineEntity 		put: #changeAdd;
		at: #debuggerPeelToFirst 		put: #down;
		at: #debuggerProceed 				put: #glamorousGo;
		at: #debuggerRestart 				put: #glamorousRestart;
		at: #debuggerReturn 				put: #refresh;
		at: #debuggerStepInto 			put: #glamorousInto;
		at: #debuggerStepOver 			put: #glamorousOver;
		at: #debuggerStepThrough 		put: #glamorousThrough;
		at: #debuggerRunToSelection 	put: #glamorousRightSide; 
		at: #debuggerWhereIs 				put: #glamorousSearch
]

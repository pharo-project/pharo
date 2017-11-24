"
GTInspectorMethodListFilter implements a filtering strategy for presentations based on method signatures (method selector and class).

The filter returns true for a method if the signature of that method is selected in the filter. To be taken into account during filtering a signature must be first added to the filter and the selected. Only signatures that have been first added can be selected. 
"
Class {
	#name : #GTInspectorMethodListFilter,
	#superclass : #GTInspectorPresentationFilter,
	#instVars : [
		'allMethodSignatures',
		'selectedMethodSignatures'
	],
	#category : #'GT-Inspector'
}

{ #category : #examples }
GTInspectorMethodListFilter class >> exampleASTBrowser [
	"An inspector showing a browser for navigating through AST nodes."
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectSignatureFor: CompiledMethod>>#gtInspectorASTIn:;
		addAndSelectSignatureFor: RBProgramNode>>#gtInspectorSourceCodeIn:).
	inspector openOn: Morph>>#initialize.
]

{ #category : #examples }
GTInspectorMethodListFilter class >> exampleCodeBrowser [
	"An inspector showing only the Meta presentation for all objects"
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectSignatureFor: Object>>#gtInspectorMetaIn:).
	inspector openOn: Morph new.
]

{ #category : #examples }
GTInspectorMethodListFilter class >> exampleFileBrowser [
	"An inspector for browsing directories and files."
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectSignatureFor: FileReference>>#gtInspectorStScriptIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorPngIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorJpegIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorItemsIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorGifIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorContentsIn:;
		addAndSelectSignatureFor: FileReference>>#gtInspectorFuelIn:).
	inspector openOn: FileSystem workingDirectory.
]

{ #category : #examples }
GTInspectorMethodListFilter class >> exampleFullInspector [
	"An inspector showing all presentations for all objects"
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectAllSignatures).
	inspector openOn: Morph new.
]

{ #category : #examples }
GTInspectorMethodListFilter class >> exampleHelpBrowser [
	"An inspector for browsing Help pages.
	
	self exampleHelpBrowser
	"
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectSignatureFor: HelpTopic>>#gtInspectorContentsIn:;
		addAndSelectSignatureFor: HelpTopic>>#gtInspectorSubtopicsIn:).
	inspector openOn: SystemHelp asHelpTopic.
]

{ #category : #examples }
GTInspectorMethodListFilter class >> exampleRawInspector [
	"An inspector showing only the Raw presentation for all objects"
	|inspector|
	
	inspector := GTInspector new.
	inspector presentationFilter: (GTInspectorMethodListFilter new
		addAndSelectSignatureFor: Object>>#gtInspectorRawIn:).
	inspector openOn: Morph new.
]

{ #category : #adding }
GTInspectorMethodListFilter >> addAllSignatures [
	
	self allPresentationMethods do: [ :aMethod | self addSignatureFor: aMethod ] 
]

{ #category : #adding }
GTInspectorMethodListFilter >> addAndSelectAllSignatures [
	
	self allPresentationMethods do: [ :aMethod | self addAndSelectSignatureFor: aMethod ] 
]

{ #category : #adding }
GTInspectorMethodListFilter >> addAndSelectSignatureFor: aMethod [
	|signature|
	
	signature := self signatureFor: aMethod.
	(allMethodSignatures includes: signature) ifFalse: [ allMethodSignatures add: signature ].
	(selectedMethodSignatures includes: signature) ifFalse: [ selectedMethodSignatures add: signature ].
	
]

{ #category : #adding }
GTInspectorMethodListFilter >> addSignatureFor: aMethod [
	|signature|
	
	signature := self signatureFor: aMethod.
	(allMethodSignatures includes: signature) ifFalse: [ allMethodSignatures add: signature ].
]

{ #category : #accessing }
GTInspectorMethodListFilter >> allPresentationMethods [

	^ (PragmaCollector allSystemPragmas select: [ :pragma | 
		pragma key == self inspectorPragmaKey ]) collect: [ :aPragma | aPragma method ]
]

{ #category : #accessing }
GTInspectorMethodListFilter >> changeFilterRequestFor: aGTInspector [

	self
		openListChooseLabeled: 'All available presentations' 
		forInspector: aGTInspector
		with: allMethodSignatures 
		select: selectedMethodSignatures 
		format: [ :aSignature | self printSignature: aSignature ]
		actionSelector: #replaceSelectedSignaturesWith:in:
]

{ #category : #initialization }
GTInspectorMethodListFilter >> initialize [
	super initialize.
	allMethodSignatures := OrderedCollection new.
	selectedMethodSignatures := OrderedCollection new.
]

{ #category : #accessing }
GTInspectorMethodListFilter >> inspectorPragmaKey [

	^ #gtInspectorPresentationOrder:
]

{ #category : #printing }
GTInspectorMethodListFilter >> printSignature: aSignature [

	^ aSignature first name, '>>', aSignature second asString
]

{ #category : #callbacks }
GTInspectorMethodListFilter >> replaceSelectedSignaturesWith: aList in: aGTInspector [

	selectedMethodSignatures := aList.
	aGTInspector update
]

{ #category : #testing }
GTInspectorMethodListFilter >> shouldDisplayPresentationCreatedBy: aMethod [

	^ selectedMethodSignatures anySatisfy: [ :aSignature | 
		aSignature first = aMethod methodClass and: [ aSignature second = aMethod selector ] ]
]

{ #category : #accessing }
GTInspectorMethodListFilter >> signatureFor: aMethod [

	^ {aMethod methodClass . aMethod selector}
]

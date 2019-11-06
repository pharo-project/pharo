"
A MCMergeOrLoadWarning is a simple warning used when amerge or a load action may override existing entities.
"
Class {
	#name : #MCMergeOrLoadWarning,
	#superclass : #Warning,
	#instVars : [
		'versions'
	],
	#category : #'Monticello-Loading'
}

{ #category : #signalling }
MCMergeOrLoadWarning class >> signalFor: aVersionCollection [
	^ self new
		versions: aVersionCollection;
		signal
]

{ #category : #actions }
MCMergeOrLoadWarning >> cancel [
	^ self resume: nil
]

{ #category : #exceptionDescription }
MCMergeOrLoadWarning >> defaultAction [
	^ ( UIManager default
		confirm: self messageText
		trueChoice: 'Load'
		falseChoice: 'Merge'
		cancelChoice: 'Cancel'
		default: nil ).
]

{ #category : #actions }
MCMergeOrLoadWarning >> load [
	^ self resume: true
]

{ #category : #actions }
MCMergeOrLoadWarning >> merge [
	^ self resume: false
]

{ #category : #accessing }
MCMergeOrLoadWarning >> messageText [
	| packageNames |
	
	packageNames := versions collect: [ :ea|  ea package name ].
		
	^ String streamContents: [:s |
		s nextPutAll: 'You are about to load new versions of the following packages'; cr;
			nextPutAll: 'that have unsaved changes in the image:'; cr.
			
		packageNames sort do: [:name |
			s cr; space; space; nextPutAll: name].
		
		s cr cr
			nextPutAll: 'If you continue, you will lose these changes:']
]

{ #category : #accessing }
MCMergeOrLoadWarning >> versions: aCollection [
	versions := aCollection
]

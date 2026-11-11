"
I am a command to show critique details.
I just show the dialog with critique description and title
"
Class {
	#name : 'ClyShowCritiqueDetailsCommand',
	#superclass : 'ClyCritiqueCommand',
	#category : 'Calypso-SystemPlugins-Critic-Browser',
	#package : 'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : 'strings' }
ClyShowCritiqueDetailsCommand >> critiqueDescription [

	^ String streamContents: [ :s |
		  s
			  nextPutAll: '[';
			  nextPutAll: self critique rule group;
			  nextPutAll: ']';
			  cr;
			  cr;
			  nextPutAll: self critique description ]
]

{ #category : 'table decoration' }
ClyShowCritiqueDetailsCommand >> description [
	^'View rationale behind the rule'
]

{ #category : 'execution' }
ClyShowCritiqueDetailsCommand >> executeRefactoring [

	self specUIManager
		title: self critique title;
		message: self critiqueDescription;
		acceptLabel: 'close';
		extent: 500 @ 500;
		openDialog
]

{ #category : 'accessing' }
ClyShowCritiqueDetailsCommand >> iconName [
	^#smallQuestion
]

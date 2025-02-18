"
I'm here to show you how to use the menu registration. See class side methods and try it with:
----------------
(PragmaMenuBuilder pragmaKeyword: 'worldMenuExample' model: nil) menu popUpInWorld
----------------

"
Class {
	#name : 'MenuRegistrationExample',
	#superclass : 'Object',
	#category : 'MenuRegistration-Example',
	#package : 'MenuRegistration',
	#tag : 'Example'
}

{ #category : 'menu declaring' }
MenuRegistrationExample class >> mostUsedToolsOn: aBuilder [
	<worldMenuExample>
	(aBuilder group: #Tools)
		withSeparatorAfter;
		order: 0;
		with: [
			(aBuilder item: #'System browser')
				action: [ (Smalltalk tools toolNamed: #browser) open  ] ;
				icon: (aBuilder iconNamed: #smallSystemBrowser).
			(aBuilder item: #Playground)
				action: [ (Smalltalk tools toolNamed: #workspace) open ];
				icon: (aBuilder iconNamed: #workspace).
			]
]

{ #category : 'menu declaring' }
MenuRegistrationExample class >> myOwnCoolMenuRegistrationOn: aBuilder [
	<worldMenuExample>
	(aBuilder item: #MyOwnCoolMenu)
		parent: #Tools; "place me as a child of the #Tools node (declared by #toolsOn:)"
		label: 'The coolest tool here';
		with: [
			(aBuilder item: #MyOwnCoolTool)
				label: 'My own cool tool also here';
				target: (Smalltalk tools toolNamed: #workspace);
				selector: #openContents: ;
				arguments: #('yep, my own cool tool can be also opened from a world sub-menu ! :)')]
]

{ #category : 'menu declaring' }
MenuRegistrationExample class >> myOwnCoolToolRegistrationOn: aBuilder [
	<worldMenuExample>
	(aBuilder item: #MyOwnCoolTool)
		parent: #Tools;
		label: 'My own cool tool';
		target: (Smalltalk tools toolNamed: #workspace) ;
		selector: #openContents: ;
		arguments: #('yep, my own cool tool can be opened from the world menu ! :)')
]

{ #category : 'menu declaring' }
MenuRegistrationExample class >> pharoItemsOn: aBuilder [
	<worldMenuExample>
	(aBuilder group: #QuitPharo)
		order: 9999;
		with: [{
				{'Save'. {Smalltalk. #saveSession}. 'save the current version of the image on disk'}.
				{'Save as...'. {self. #saveAs}. 'save the current version of the image on disk under a new name.'}.
				{'Save and quit'. {self. #saveAndQuit}. 'save the current image on disk, and quit Pharo.'}.
	 			{'Quit'. {self. #quitSession}. 'quit Pharo.'}
			} do: [:triplet |
					(aBuilder item: triplet first asSymbol)
						target: triplet second first;
				 		selector: triplet second second;
						help: (triplet size > 2 ifTrue: [triplet third] ifFalse: [nil])]]
]

{ #category : 'menu declaring' }
MenuRegistrationExample class >> pragmaKeyword [
	^ #worldMenuExample
]

{ #category : 'menu actions' }
MenuRegistrationExample class >> quitSession [

	Smalltalk
		snapshot: (MorphicUIManager new
				 confirm: 'Save changes before quitting?' translated
				 orCancel: [ ^ self ])
		andQuit: true
]

{ #category : 'menu actions' }
MenuRegistrationExample class >> saveAndQuit [
	Smalltalk snapshot: true andQuit: true
]

{ #category : 'menu actions' }
MenuRegistrationExample class >> saveAs [
	Smalltalk saveAs
]

{ #category : 'menu declaring' }
MenuRegistrationExample class >> theme [
	"Answer the ui theme that provides controls."

	^ Smalltalk ui theme
]

{ #category : 'menu declaring' }
MenuRegistrationExample class >> toolsOn: aBuilder [
	<worldMenuExample>
	(aBuilder item: #Tools)
		order: 1.0;
		iconName: #smallDoIt;
		withSeparatorAfter;
		with: [
			(aBuilder item: #Transcript)
				action: [ (Smalltalk tools toolNamed: #transcript) open] ;
				icon: (aBuilder iconNamed: #smallDebug).
			(aBuilder item: #'File Browser')
				action: [ (Smalltalk tools toolNamed: #fileList) open] ;
				icon: (self  iconNamed: #smallWindow).
			(aBuilder item: #'Process Browser')
				selector: #openProcessBrowser;
				icon: (aBuilder iconNamed: #processBrowser);
				withSeparatorAfter]
]

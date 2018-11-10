"
I'm a command to perform a fetch operation on all registered projects.
"
Class {
	#name : #IceTipFetchAllProjectCommand,
	#superclass : #IceTipRepositoryCommand,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipFetchAllProjectCommand class >> defaultHelp [
	^ 'Fetch (bring new commits) from all repositories in your list.'
]

{ #category : #accessing }
IceTipFetchAllProjectCommand class >> defaultMenuIconName [
	^ #smallUpdate
]

{ #category : #accessing }
IceTipFetchAllProjectCommand class >> defaultMenuItemName [
	^ 'Fetch all'
]

{ #category : #accessing }
IceTipFetchAllProjectCommand class >> toolbarRepositoryListActivation [
	<classAnnotation>
	
	^ IceTipToolbarActivation 
		byRootGroupItemFor: IceTipRepositoryListContext
		order: 20
]

{ #category : #execution }
IceTipFetchAllProjectCommand >> execute [
	| repositories |
	(UIManager default confirm: 'This operation can take some times depending on the size and number of your repositories. Are you sure you want to fetch all repositories?' label: 'Proceed?')
		ifFalse: [ ^ self ].
		
	repositories := IceRepository registry.
	UIManager default
		informUserDuring: [ :bar | 
			bar max: repositories size.
			bar value: 1.
			repositories
				withIndexDo: [ :each :index | 
					bar label: ('Fetching incoming commits of {1}' format: {each name}).
					bar value: index + 1.
					each fetch ] ].
			
	"Inform an unespecified change to force a refresh"
	Iceberg announcer announce: IceRepositoryModified new
]

Extension { #name : #GTSpotter }

{ #category : #'*Tool-Catalog' }
GTSpotter >> spotterCatalogProjectsFor: aStep [
	<spotterOrder: 90>
	
	CatalogSettings displayCatalogProjectsInSpotter ifFalse: [ ^self ].
	
	aStep listProcessor			 
			allCandidates: [ CatalogProvider default projects ];
			title: 'Catalog Projects';
			itemName: [ :item | item name ];
			itemIcon: [ :item | CatalogBrowser iconFor: item ];
			filter: GTFilterSubstring;
			actLogic: [ :item | 
				aStep exit.
				(UIManager default confirm: 'Would you like to install ', item name, '?') 
					 ifTrue: [ item installStableVersion.
								  UIManager inform: item name, ' installed'. ]];
			wantsToDisplayOnEmptyQuery: true
]

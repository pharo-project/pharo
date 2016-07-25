entryCompletion
entitiesEntryCompletion
	^ EntryCompletion new
				dataSourceBlock: [:currText | self possibleEntities collect: [:each | 
		each perform: self displaySymbol ]];
				chooseBlock: [ :selectedName | 
					self entity: (self possibleEntities detect: [ :each | 
						(each perform: self displaySymbol) = selectedName ])];
				filterBlock: [:currEntity :currText | 
					currEntity asUppercase includesSubstring: currText asString asUppercase].
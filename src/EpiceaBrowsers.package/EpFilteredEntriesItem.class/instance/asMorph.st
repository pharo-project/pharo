converting
asMorph

	^ (self panelWidgetWithAll: { self 
			smallStringWidgetFor: '(', filteredEntriesCount printString, ' filtered entries)'
			color: Color transparent })
			wrapCentering: #center;
			cellPositioning: #center;
			yourself.
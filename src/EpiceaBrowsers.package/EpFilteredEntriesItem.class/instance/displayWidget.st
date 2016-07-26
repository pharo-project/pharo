populating browser
displayWidget

	^ (self panelWidgetWithAll: { self 
			smallStringWidgetFor: '(', self entries size printString, ' filtered entries)'
			color: Color transparent })
			wrapCentering: #center;
			cellPositioning: #center;
			yourself.
specs
defaultSpec
	<spec>
	
	^ SpecLayout composed
			newRow: [:r |
					r 
						add: #changeSorterLeft;
						addSplitter;
						add: #changeSorterRight ]
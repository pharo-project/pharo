items creation
buildSearchModeDropListIn: aWindow
			^(self theme 
				newDropListIn: aWindow
				for:self				
				list: #searchModesList
				getSelected: #currentSearchMode
				setSelected: #currentSearchMode:
				getEnabled: nil
				useIndex: false 
				help: self searchModeHelpText) hResizing: #rigid; width: 120; yourself

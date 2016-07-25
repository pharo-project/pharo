user interface
addModelItemsToWindowMenu: aMenu
	"Add model-related items to the window menu"

	aMenu
		addLine;
		add: 'Clean all manifest' target: checker selector: #cleanAllManifest;
		add: 'Reapply all  rules' target: self selector: #reapplyAllRules